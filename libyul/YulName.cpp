/*
	This file is part of solidity.

	solidity is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	solidity is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with solidity.  If not, see <http://www.gnu.org/licenses/>.
*/
// SPDX-License-Identifier: GPL-3.0

#include <libyul/backends/evm/EVMDialect.h>

#include <libyul/YulName.h>
#include <libyul/Dialect.h>
#include <libyul/Exceptions.h>

namespace solidity::yul
{

YulNameRepository::YulNameRepository(solidity::yul::Dialect const& _dialect):
	m_dialect(_dialect)
{
	defineName("");

	auto const& builtinNames = _dialect.builtinNames();
	m_nBuiltin = builtinNames.size() + 1;  // +1 verbatim gets special treatment
	m_predefined.verbatim = defineName("verbatim");
	for(auto const& label: builtinNames)
	{
		auto const name = defineName(label);
		if(auto const* function = m_dialect.builtin(label))
			m_builtinFunctions[name] = convertBuiltinFunction(*function);
	}

	for(auto const& type : _dialect.types)
		m_dialectTypes.emplace_back(defineName(type), type);

	m_predefined.boolType = nameOfType(_dialect.boolType);
	m_predefined.defaultType = nameOfType(_dialect.defaultType);
	m_predefined.dataoffset = nameOfBuiltin("dataoffset");
	m_predefined.datasize = nameOfBuiltin("datasize");
	m_predefined.selfdestruct = nameOfBuiltin("selfdestruct");
	m_predefined.tstore = nameOfBuiltin("tstore");
	m_predefined.memoryguard = nameOfBuiltin("memoryguard");
	m_predefined.eq = nameOfBuiltin("eq");
	m_predefined.add = nameOfBuiltin("add");
	m_predefined.sub = nameOfBuiltin("sub");

	{
		auto types = m_dialectTypes;
		if (types.empty())
			types.emplace_back(0, "");
		for (auto const& [typeName, typeLabel]: m_dialectTypes)
		{
			if (auto const* discardFunction = m_dialect.discardFunction(typeLabel))
				m_discardFunctions.emplace_back(nameOfBuiltin(discardFunction->name));
			else
				m_discardFunctions.emplace_back(std::nullopt);

			if (auto const* equalityFunction = m_dialect.equalityFunction(typeLabel))
				m_equalityFunctions.emplace_back(nameOfBuiltin(equalityFunction->name));
			else
				m_equalityFunctions.emplace_back(std::nullopt);

			if (auto const* booleanNegationFunction = m_dialect.booleanNegationFunction())
				m_booleanNegationFunction = nameOfBuiltin(booleanNegationFunction->name);
			else
				m_booleanNegationFunction = std::nullopt;

			if (auto const* memStoreFunction = m_dialect.memoryStoreFunction(typeLabel))
				m_memoryStoreFunctions.emplace_back(nameOfBuiltin(memStoreFunction->name));
			else
				m_memoryStoreFunctions.emplace_back(std::nullopt);

			if (auto const* memLoadFunction = m_dialect.memoryLoadFunction(typeLabel))
				m_memoryLoadFunctions.emplace_back(nameOfBuiltin(memLoadFunction->name));
			else
				m_memoryLoadFunctions.emplace_back(std::nullopt);

			if (auto const* storageStoreFunction = m_dialect.storageStoreFunction(typeLabel))
				m_storageStoreFunctions.emplace_back(nameOfBuiltin(storageStoreFunction->name));
			else
				m_storageStoreFunctions.emplace_back(std::nullopt);

			if (auto const* storageLoadFunction = m_dialect.storageLoadFunction(typeLabel))
				m_storageLoadFunctions.emplace_back(nameOfBuiltin(storageLoadFunction->name));
			else
				m_storageLoadFunctions.emplace_back(std::nullopt);

			m_hashFunctions.emplace_back(nameOfBuiltin(m_dialect.hashFunction(typeLabel)));
		}
	}

	m_predefined.GHOST = defineName("GHOST[]");
	m_predefined.placeholder_zero = defineName("@ 0");
	m_predefined.placeholder_one = defineName("@ 1");
	m_predefined.placeholder_thirtytwo = defineName("@ 32");
}

YulNameRepository::BuiltinFunction const* YulNameRepository::builtin(YulName const _name) const
{
	auto const baseName = baseNameOf(_name);
	// 0 is empty, then nPredefined indices for builtins
	if (isBuiltinName(baseName))
	{
		auto const it = m_builtinFunctions.find(_name);
		if (it != m_builtinFunctions.end())
			return &it->second;
		else
			return nullptr;
	}
	return nullptr;
}

std::string_view YulNameRepository::labelOf(YulName const _name) const
{
	// todo to some degree (make it unambiguous and in one go)
	if (!isDerivedName(_name))
	{
		// if the parent is directly a defined label, we take that one
		return m_definedLabels[std::get<0>(m_names[_name])];
	}
	else
	{
		auto const it = m_derivedNameCache.find(_name);
		yulAssert(it != m_derivedNameCache.end(), "Derived name was not yet defined.");
		return it->second;
		/*auto [it, emplaced] = m_derivedNameCache.try_emplace(_name, "");
		size_t indirections{0};
		auto _baseName = _name;
		if (emplaced)
		{
			// otherwise, traverse the linked list until we find a defined label and prepend indices
			while (isDerivedName(_name))
			{
				it->second.insert(0, "_" + std::to_string(_baseName));
				_baseName = std::get<0>(m_names[_baseName]);
				++indirections;
			}
			it->second.insert(0, m_definedLabels[std::get<0>(m_names[_baseName])]);
		}
		if (std::get<0>(m_names[_baseName]) == m_predefined.GHOST)
			it->second = fmt::format("GHOST[{}]", indirections);
		if (std::get<0>(m_names[_baseName]) == m_predefined.verbatim)
		{
			auto const* function = builtin(_name);
			yulAssert(function);
			it->second = function->data->name;
		}
		return it->second;*/
	}
}

YulNameRepository::BuiltinFunction const* YulNameRepository::discardFunction(YulName const _type) const
{
	auto const typeIndex = indexOfType(_type);
	auto const& functionName = m_discardFunctions[typeIndex];
	if(!functionName)
		return nullptr;
	return builtin(*functionName);
}

YulNameRepository::BuiltinFunction const* YulNameRepository::equalityFunction(YulName _type) const
{
	auto const typeIndex = indexOfType(_type);
	auto const& functionName = m_equalityFunctions[typeIndex];
	if(!functionName)
		return nullptr;
	return builtin(*functionName);
}

YulNameRepository::BuiltinFunction const* YulNameRepository::booleanNegationFunction() const
{
	if (!m_booleanNegationFunction)
		return nullptr;
	return builtin(*m_booleanNegationFunction);
}

YulNameRepository::BuiltinFunction const* YulNameRepository::memoryLoadFunction(YulName _type) const
{
	auto const typeIndex = indexOfType(_type);
	auto const& functionName = m_memoryLoadFunctions[typeIndex];
	if(!functionName)
		return nullptr;
	return builtin(*functionName);
}

YulNameRepository::BuiltinFunction const* YulNameRepository::memoryStoreFunction(YulName _type) const
{
	auto const typeIndex = indexOfType(_type);
	auto const& functionName = m_memoryStoreFunctions[typeIndex];
	if(!functionName)
		return nullptr;
	return builtin(*functionName);
}

YulNameRepository::BuiltinFunction const* YulNameRepository::storageLoadFunction(YulName _type) const
{
	auto const typeIndex = indexOfType(_type);
	auto const& functionName = m_storageLoadFunctions[typeIndex];
	if(!functionName)
		return nullptr;
	return builtin(*functionName);
}

YulNameRepository::BuiltinFunction const* YulNameRepository::storageStoreFunction(YulName _type) const
{
	auto const typeIndex = indexOfType(_type);
	auto const& functionName = m_storageStoreFunctions[typeIndex];
	if(!functionName)
		return nullptr;
	return builtin(*functionName);
}

YulName YulNameRepository::hashFunction(YulName _type) const
{
	auto const typeIndex = indexOfType(_type);
	return m_hashFunctions[typeIndex];
}

bool YulNameRepository::isBuiltinName(YulName _name) const
{
	auto const baseName = baseNameOf(_name);
	return baseName > 0 && baseName < 1 + m_nBuiltin;
}

YulNameRepository::BuiltinFunction YulNameRepository::convertBuiltinFunction(yul::BuiltinFunction const& _builtin) const
{
	BuiltinFunction result;
	result.name = nameOfBuiltin(_builtin.name);
	for (auto const& type : _builtin.parameters)
		result.parameters.push_back(nameOfType(type));
	for (auto const& type : _builtin.returns)
		result.returns.push_back(nameOfType(type));
	result.data = &_builtin;
	return result;
}

YulName YulNameRepository::nameOfBuiltin(std::string_view const builtin) const
{
	for (size_t i = 0; i < 1 + m_nBuiltin; ++i)
		if (baseLabelOf(std::get<0>(m_names[i])) == builtin)
			return i;
	return emptyName();
}

YulName YulNameRepository::nameOfType(std::string_view const _type) const
{
	if (!m_dialectTypes.empty())
	{
		for(auto const& m_dialectType : m_dialectTypes)
			if (std::get<1>(m_dialectType) == _type)
				return std::get<0>(m_dialectType);
		yulAssert(false, "only defined for (some) dialect types");
	}
	else
		return emptyName();
}

size_t YulNameRepository::indexOfType(YulName const _type) const
{
	if (m_dialectTypes.empty())
		return 0;
	auto const it = std::find_if(m_dialectTypes.begin(), m_dialectTypes.end(), [&](auto const& element) { return std::get<0>(element) == _type; });
	yulAssert(it != m_dialectTypes.end());
	return static_cast<size_t>(std::distance(m_dialectTypes.begin(), it));
}

Dialect const& YulNameRepository::dialect() const
{
	return m_dialect;
}

YulName YulNameRepository::defineName(std::string_view const _label)
{
	if (auto builtin = m_dialect.builtin(_label))
	{
		if (builtin->name.substr(0, std::string_view("verbatim").size()) == "verbatim")
		{
			auto const key = std::make_tuple(builtin->parameters.size(), builtin->returns.size());
			auto [it, emplaced] = m_verbatimNames.try_emplace(key);
			if (emplaced)
				it->second = deriveName(predefined().verbatim);
			return it->second;
		}
		else
			return nameOfBuiltin(_label);
	}
	else
	{
		m_definedLabels.emplace_back(_label);
		m_names.emplace_back(m_definedLabels.size() - 1, false);
		return m_index++;
	}
}
bool YulNameRepository::isType(YulName const _name) const {
	return std::find_if(
    	m_dialectTypes.begin(),
	    m_dialectTypes.end(),
	    [&](auto const& element) { return std::get<0>(element) == _name; }
    ) != m_dialectTypes.end();
}

bool YulNameRepository::isEvmDialect() const { return dynamic_cast<EVMDialect const*>(&m_dialect) != nullptr; }

}
