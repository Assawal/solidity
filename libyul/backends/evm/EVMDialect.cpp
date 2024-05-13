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
/**
 * Yul dialects for EVM.
 */

#include <libyul/backends/evm/EVMDialect.h>

#include <libevmasm/Instruction.h>
#include <libevmasm/SemanticInformation.h>
#include <liblangutil/Exceptions.h>
#include <libsolutil/StringUtils.h>
#include <libyul/AST.h>
#include <libyul/AsmAnalysisInfo.h>
#include <libyul/AsmParser.h>
#include <libyul/Exceptions.h>
#include <libyul/Object.h>
#include <libyul/Utilities.h>
#include <libyul/backends/evm/AbstractAssembly.h>

#include <range/v3/view/reverse.hpp>
#include <range/v3/view/tail.hpp>

#include <range/v3/range/conversion.hpp>
#include <range/v3/view/transform.hpp>
#include <regex>

using namespace std::string_literals;
using namespace solidity;
using namespace solidity::yul;
using namespace solidity::util;

namespace
{

std::pair<YulName, BuiltinFunctionForEVM> createEVMFunction(
	langutil::EVMVersion _evmVersion,
	YulName const _name,
	evmasm::Instruction _instruction
)
{
	evmasm::InstructionInfo info = evmasm::instructionInfo(_instruction, _evmVersion);
	BuiltinFunctionForEVM f;
	f.name = _name;
	f.parameters.resize(static_cast<size_t>(info.args));
	f.returns.resize(static_cast<size_t>(info.ret));
	f.sideEffects = EVMDialect::sideEffectsOfInstruction(_instruction);
	if (evmasm::SemanticInformation::terminatesControlFlow(_instruction))
	{
		f.controlFlowSideEffects.canContinue = false;
		if (evmasm::SemanticInformation::reverts(_instruction))
		{
			f.controlFlowSideEffects.canTerminate = false;
			f.controlFlowSideEffects.canRevert = true;
		}
		else
		{
			f.controlFlowSideEffects.canTerminate = true;
			f.controlFlowSideEffects.canRevert = false;
		}
	}
	f.isMSize = _instruction == evmasm::Instruction::MSIZE;
	f.literalArguments.clear();
	f.instruction = _instruction;
	f.generateCode = [_instruction](
		FunctionCall const&,
		AbstractAssembly& _assembly,
		BuiltinContext&
	) {
		_assembly.appendInstruction(_instruction);
	};

	YulName name = f.name;
	return {name, std::move(f)};
}

std::pair<YulName, BuiltinFunctionForEVM> createFunction(
	YulName const _name,
	size_t _params,
	size_t _returns,
	SideEffects _sideEffects,
	std::vector<std::optional<LiteralKind>> _literalArguments,
	std::function<void(FunctionCall const&, AbstractAssembly&, BuiltinContext&)> _generateCode
)
{
	yulAssert(_literalArguments.size() == _params || _literalArguments.empty(), "");

	BuiltinFunctionForEVM f;
	f.name = _name;
	f.parameters.resize(_params);
	f.returns.resize(_returns);
	f.sideEffects = std::move(_sideEffects);
	f.literalArguments = std::move(_literalArguments);
	f.isMSize = false;
	f.instruction = {};
	f.generateCode = std::move(_generateCode);
	return {_name, f};
}

std::set<YulNameLabel, std::less<>> createReservedIdentifiers(langutil::EVMVersion _evmVersion)
{
	// TODO remove this in 0.9.0. We allow creating functions or identifiers in Yul with the name
	// basefee for VMs before london.
	auto baseFeeException = [&](evmasm::Instruction _instr) -> bool
	{
		return _instr == evmasm::Instruction::BASEFEE && _evmVersion < langutil::EVMVersion::london();
	};

	// TODO remove this in 0.9.0. We allow creating functions or identifiers in Yul with the name
	// blobbasefee for VMs before cancun.
	auto blobBaseFeeException = [&](evmasm::Instruction _instr) -> bool
	{
		return _instr == evmasm::Instruction::BLOBBASEFEE && _evmVersion < langutil::EVMVersion::cancun();
	};

	// TODO remove this in 0.9.0. We allow creating functions or identifiers in Yul with the name
	// mcopy for VMs before london.
	auto mcopyException = [&](evmasm::Instruction _instr) -> bool
	{
		return _instr == evmasm::Instruction::MCOPY && _evmVersion < langutil::EVMVersion::cancun();
	};

	// TODO remove this in 0.9.0. We allow creating functions or identifiers in Yul with the name
	// prevrandao for VMs before paris.
	auto prevRandaoException = [&](std::string const& _instrName) -> bool
	{
		// Using string comparison as the opcode is the same as for "difficulty"
		return _instrName == "prevrandao" && _evmVersion < langutil::EVMVersion::paris();
	};

	// TODO remove this in 0.9.0. We allow creating functions or identifiers in Yul with the name
	// blobhash for VMs before cancun.
	auto blobHashException = [&](evmasm::Instruction _instr) -> bool
	{
		return _instr == evmasm::Instruction::BLOBHASH && _evmVersion < langutil::EVMVersion::cancun();
	};
	// TODO remove this in 0.9.0. We allow creating functions or identifiers in Yul with the names
	// tstore or tload for VMs before cancun.
	auto transientStorageException = [&](evmasm::Instruction _instr) -> bool
	{
		return
			_evmVersion < langutil::EVMVersion::cancun() &&
			(_instr == evmasm::Instruction::TSTORE || _instr == evmasm::Instruction::TLOAD);
	};

	std::set<YulNameLabel, std::less<>> reserved;
	for (auto const& instr: evmasm::c_instructions)
	{
		std::string name = toLower(instr.first);
		if (
			!baseFeeException(instr.second) &&
			!prevRandaoException(name) &&
			!blobHashException(instr.second) &&
			!blobBaseFeeException(instr.second) &&
			!mcopyException(instr.second) &&
			!transientStorageException(instr.second)
		)
			reserved.emplace(name);
	}
	reserved += std::vector<YulNameLabel>{
		"linkersymbol",
		"datasize",
		"dataoffset",
		"datacopy",
		"setimmutable",
		"loadimmutable",
	};
	return reserved;
}

std::map<YulName, BuiltinFunctionForEVM> createBuiltins(langutil::EVMVersion _evmVersion, bool _objectAccess)
{
	auto &_nameResolver = YulNameRegistry::instance();
	// Exclude prevrandao as builtin for VMs before paris and difficulty for VMs after paris.
	auto prevRandaoException = [&](std::string const& _instrName) -> bool
	{
		return (_instrName == "prevrandao" && _evmVersion < langutil::EVMVersion::paris()) || (_instrName == "difficulty" && _evmVersion >= langutil::EVMVersion::paris());
	};

	std::map<YulName, BuiltinFunctionForEVM> builtins;
	for (auto const& instr: evmasm::c_instructions)
	{
		std::string name = toLower(instr.first);
		auto const opcode = instr.second;

		if (
			!evmasm::isDupInstruction(opcode) &&
			!evmasm::isSwapInstruction(opcode) &&
			!evmasm::isPushInstruction(opcode) &&
			opcode != evmasm::Instruction::JUMP &&
			opcode != evmasm::Instruction::JUMPI &&
			opcode != evmasm::Instruction::JUMPDEST &&
			_evmVersion.hasOpcode(opcode) &&
			!prevRandaoException(name)
		)
			builtins.emplace(createEVMFunction(_evmVersion, _nameResolver.idOf(name), opcode));
	}

	if (_objectAccess)
	{
		builtins.emplace(createFunction(
			_nameResolver.idOf("linkersymbol"), 1, 1, SideEffects{}, {LiteralKind::String}, [&_nameResolver](
			FunctionCall const& _call,
			AbstractAssembly& _assembly,
			BuiltinContext&
		) {
			yulAssert(_call.arguments.size() == 1, "");
			Expression const& arg = _call.arguments.front();
			_assembly.appendLinkerSymbol(_nameResolver.resolve_s(std::get<Literal>(arg).value));
		}));

		builtins.emplace(createFunction(
			_nameResolver.idOf("memoryguard"),
			1,
			1,
			SideEffects{},
			{LiteralKind::Number},
			[](
				FunctionCall const& _call,
				AbstractAssembly& _assembly,
				BuiltinContext&
			) {
				yulAssert(_call.arguments.size() == 1, "");
				Literal const* literal = std::get_if<Literal>(&_call.arguments.front());
				yulAssert(literal, "");
				_assembly.appendConstant(valueOfLiteral(*literal));
			})
		);

		builtins.emplace(createFunction(
			_nameResolver.idOf("datasize"), 1, 1, SideEffects{}, {LiteralKind::String}, [&_nameResolver](
			FunctionCall const& _call,
			AbstractAssembly& _assembly,
			BuiltinContext& _context
		) {
			yulAssert(_context.currentObject, "No object available.");
			yulAssert(_call.arguments.size() == 1, "");
			Expression const& arg = _call.arguments.front();
			YulName dataName = std::get<Literal>(arg).value;
			if (_context.currentObject->name == dataName)
				_assembly.appendAssemblySize();
			else
			{
			std::vector<size_t> subIdPath =
					_context.subIDs.count(dataName) == 0 ?
						_context.currentObject->pathToSubObject(dataName) :
						std::vector<size_t>{_context.subIDs.at(dataName)};
				yulAssert(!subIdPath.empty(), "Could not find assembly object <" + _nameResolver.resolve_s(dataName) + ">.");
				_assembly.appendDataSize(subIdPath);
			}
		}));
		builtins.emplace(createFunction(
			_nameResolver.idOf("dataoffset"), 1, 1, SideEffects{}, {LiteralKind::String}, [&_nameResolver](
			FunctionCall const& _call,
			AbstractAssembly& _assembly,
			BuiltinContext& _context
		) {
			yulAssert(_context.currentObject, "No object available.");
			yulAssert(_call.arguments.size() == 1, "");
			Expression const& arg = _call.arguments.front();
			YulName dataName = std::get<Literal>(arg).value;
			if (_context.currentObject->name == dataName)
				_assembly.appendConstant(0);
			else
			{
			std::vector<size_t> subIdPath =
					_context.subIDs.count(dataName) == 0 ?
						_context.currentObject->pathToSubObject(dataName) :
						std::vector<size_t>{_context.subIDs.at(dataName)};
				yulAssert(!subIdPath.empty(), "Could not find assembly object <" + _nameResolver.resolve_s(dataName) + ">.");
				_assembly.appendDataOffset(subIdPath);
			}
		}));
		builtins.emplace(createFunction(
			_nameResolver.idOf("datacopy"),
			3,
			0,
			SideEffects{
				false,               // movable
				true,                // movableApartFromEffects
				false,               // canBeRemoved
				false,               // canBeRemovedIfNotMSize
				true,                // cannotLoop
				SideEffects::None,   // otherState
				SideEffects::None,   // storage
				SideEffects::Write,  // memory
				SideEffects::None    // transientStorage
			},
			{},
			[](
				FunctionCall const&,
				AbstractAssembly& _assembly,
				BuiltinContext&
			) {
				_assembly.appendInstruction(evmasm::Instruction::CODECOPY);
			}
		));
		builtins.emplace(createFunction(
			_nameResolver.idOf("setimmutable"),
			3,
			0,
			SideEffects{
				false,               // movable
				false,               // movableApartFromEffects
				false,               // canBeRemoved
				false,               // canBeRemovedIfNotMSize
				true,                // cannotLoop
				SideEffects::None,   // otherState
				SideEffects::None,   // storage
				SideEffects::Write,  // memory
				SideEffects::None    // transientStorage
			},
			{std::nullopt, LiteralKind::String, std::nullopt},
			[&_nameResolver](
				FunctionCall const& _call,
				AbstractAssembly& _assembly,
				BuiltinContext&
			) {
				yulAssert(_call.arguments.size() == 3, "");
				YulName identifier = std::get<Literal>(_call.arguments[1]).value;
				_assembly.appendImmutableAssignment(_nameResolver.resolve_s(identifier));
			}
		));
		builtins.emplace(createFunction(
			_nameResolver.idOf("loadimmutable"),
			1,
			1,
			SideEffects{},
			{LiteralKind::String},
			[&_nameResolver](
				FunctionCall const& _call,
				AbstractAssembly& _assembly,
				BuiltinContext&
			) {
				yulAssert(_call.arguments.size() == 1, "");
				_assembly.appendImmutable(_nameResolver.resolve_s(std::get<Literal>(_call.arguments.front()).value));
			}
		));
	}
	return builtins;
}

std::regex const& verbatimPattern()
{
	std::regex static const pattern{"verbatim_([1-9]?[0-9])i_([1-9]?[0-9])o"};
	return pattern;
}

}


EVMDialect::EVMDialect(langutil::EVMVersion _evmVersion, bool _objectAccess):
	m_objectAccess(_objectAccess),
	m_evmVersion(_evmVersion),
	m_functions(createBuiltins(_evmVersion, _objectAccess)),
	m_reservedLabels(createReservedIdentifiers(_evmVersion)),
	m_reserved(m_reservedLabels | ranges::views::transform([](YulNameLabel const& _label) { return YulNameRegistry::instance().idOf(_label); }) | ranges::to<std::set>)
{
}

BuiltinFunctionForEVM const* EVMDialect::builtin(YulName _name) const
{
	if (m_objectAccess)
	{
		std::smatch match;
		if (regex_match(YulNameRegistry::instance().resolve_s(_name), match, verbatimPattern()))
			return verbatimFunction(stoul(match[1]), stoul(match[2]));
	}
	auto it = m_functions.find(_name);
	if (it != m_functions.end())
		return &it->second;
	else
		return nullptr;
}

bool EVMDialect::reservedIdentifier(YulName _name) const
{
	if (m_objectAccess)
		if (YulNameRegistry::instance().resolve(_name).substr(0, "verbatim"s.size()) == "verbatim")
			return true;
	return m_reserved.count(_name) != 0;
}

bool EVMDialect::reservedIdentifier(std::string_view const _label) const {
	if(m_objectAccess && _label.substr(0, "verbatim"s.size()) == "verbatim")
		return true;
	return m_reservedLabels.count(_label) > 0;
}

EVMDialect const& EVMDialect::strictAssemblyForEVM(langutil::EVMVersion _version)
{
	static std::map<langutil::EVMVersion, std::unique_ptr<EVMDialect const>> dialects;
	static YulStringRepository::ResetCallback callback{[&] { dialects.clear(); }};
	if (!dialects[_version])
		dialects[_version] = std::make_unique<EVMDialect>(_version, false);
	return *dialects[_version];
}

EVMDialect const& EVMDialect::strictAssemblyForEVMObjects(langutil::EVMVersion _version)
{
	static std::map<langutil::EVMVersion, std::unique_ptr<EVMDialect const>> dialects;
	static YulStringRepository::ResetCallback callback{[&] { dialects.clear(); }};
	if (!dialects[_version])
		dialects[_version] = std::make_unique<EVMDialect>(_version, true);
	return *dialects[_version];
}

SideEffects EVMDialect::sideEffectsOfInstruction(evmasm::Instruction _instruction)
{
	auto translate = [](evmasm::SemanticInformation::Effect _e) -> SideEffects::Effect
	{
		return static_cast<SideEffects::Effect>(_e);
	};

	return SideEffects{
		evmasm::SemanticInformation::movable(_instruction),
		evmasm::SemanticInformation::movableApartFromEffects(_instruction),
		evmasm::SemanticInformation::canBeRemoved(_instruction),
		evmasm::SemanticInformation::canBeRemovedIfNoMSize(_instruction),
		true, // cannotLoop
		translate(evmasm::SemanticInformation::otherState(_instruction)),
		translate(evmasm::SemanticInformation::storage(_instruction)),
		translate(evmasm::SemanticInformation::memory(_instruction)),
		translate(evmasm::SemanticInformation::transientStorage(_instruction)),
	};
}

BuiltinFunctionForEVM const* EVMDialect::verbatimFunction(size_t _arguments, size_t _returnVariables) const
{
	std::pair<size_t, size_t> key{_arguments, _returnVariables};
	std::shared_ptr<BuiltinFunctionForEVM const>& function = m_verbatimFunctions[key];
	if (!function)
	{
		BuiltinFunctionForEVM builtinFunction = createFunction(
				  YulNameRegistry::instance().idOf(
					  "verbatim_" + std::to_string(_arguments) + "i_" + std::to_string(_returnVariables) + "o"),
			1 + _arguments,
			_returnVariables,
			SideEffects::worst(),
			std::vector<std::optional<LiteralKind>>{LiteralKind::String} + std::vector<std::optional<LiteralKind>>(_arguments),
			[=](
				FunctionCall const& _call,
				AbstractAssembly& _assembly,
				BuiltinContext&
			) {
				yulAssert(_call.arguments.size() == (1 + _arguments), "");
				Expression const& bytecode = _call.arguments.front();

				_assembly.appendVerbatim(
					asBytes(YulNameRegistry::instance().resolve(std::get<Literal>(bytecode).value)),
					_arguments,
					_returnVariables
				);
			}
		).second;
		builtinFunction.isMSize = true;
		function = std::make_shared<BuiltinFunctionForEVM const>(std::move(builtinFunction));
	}
	return function.get();
}

EVMDialectTyped::EVMDialectTyped(langutil::EVMVersion _evmVersion, bool _objectAccess):
	EVMDialect(_evmVersion, _objectAccess)
{
	auto const idOf = [](std::string_view const label) {
		return YulNameRegistry::instance().idOf(label);
	};
	defaultType = idOf("u256");
	boolType = idOf("bool");
	types = {defaultType, boolType};

	// Set all types to ``defaultType``
	for (auto& fun: m_functions)
	{
		for (auto& p: fun.second.parameters)
			p = defaultType;
		for (auto& r: fun.second.returns)
			r = defaultType;
	}

	for(auto const comparison : {"lt", "gt", "slt", "sgt", "eq"}) {
		m_functions[idOf(comparison)].returns = {boolType};
	}

	// "not" and "bitnot" replace "iszero" and "not"
	m_functions[idOf("bitnot")] = m_functions[idOf("not")];
	m_functions[idOf("bitnot")].name = idOf("bitnot");
	m_functions[idOf("not")] = m_functions[idOf("iszero")];
	m_functions[idOf("not")].name = idOf("not");
	m_functions[idOf("not")].returns = {idOf("bool")};
	m_functions[idOf("not")].parameters = {idOf("bool")};
	m_functions.erase(idOf("iszero"));

	m_functions[idOf("bitand")] = m_functions[idOf("and")];
	m_functions[idOf("bitand")].name = idOf("bitand");
	m_functions[idOf("bitor")] = m_functions[idOf("or")];
	m_functions[idOf("bitor")].name = idOf("bitor");
	m_functions[idOf("bitxor")] = m_functions[idOf("xor")];
	m_functions[idOf("bitxor")].name = idOf("bitxor");
	m_functions[idOf("and")].parameters = {idOf("bool"), idOf("bool")};
	m_functions[idOf("and")].returns = {idOf("bool")};
	m_functions[idOf("or")].parameters = {idOf("bool"), idOf("bool")};
	m_functions[idOf("or")].returns = {idOf("bool")};
	m_functions[idOf("xor")].parameters = {idOf("bool"), idOf("bool")};
	m_functions[idOf("xor")].returns = {idOf("bool")};
	m_functions[idOf("popbool")] = m_functions[idOf("pop")];
	m_functions[idOf("popbool")].name = idOf("popbool");
	m_functions[idOf("popbool")].parameters = {idOf("bool")};
	m_functions.insert(createFunction(idOf("bool_to_u256"), 1, 1, {}, {}, [](
		FunctionCall const&,
		AbstractAssembly&,
		BuiltinContext&
	) {}));
	m_functions[idOf("bool_to_u256")].parameters = {idOf("bool")};
	m_functions[idOf("bool_to_u256")].returns = {idOf("u256")};
	m_functions.insert(createFunction(idOf("u256_to_bool"), 1, 1, {}, {}, [](
		FunctionCall const&,
		AbstractAssembly& _assembly,
		BuiltinContext&
	) {
		// TODO this should use a Panic.
		// A value larger than 1 causes an invalid instruction.
		_assembly.appendConstant(2);
		_assembly.appendInstruction(evmasm::Instruction::DUP2);
		_assembly.appendInstruction(evmasm::Instruction::LT);
		AbstractAssembly::LabelID inRange = _assembly.newLabelId();
		_assembly.appendJumpToIf(inRange);
		_assembly.appendInstruction(evmasm::Instruction::INVALID);
		_assembly.appendLabel(inRange);
	}));
	m_functions[idOf("u256_to_bool")].parameters = {idOf("u256")};
	m_functions[idOf("u256_to_bool")].returns = {idOf("bool")};
}

BuiltinFunctionForEVM const* EVMDialectTyped::discardFunction(YulName _type) const
{
	if (_type == boolType)
		return builtin(YulNameRegistry::instance().idOf("popbool"));
	else
	{
		yulAssert(_type == defaultType, "");
		return builtin(YulNameRegistry::instance().idOf("pop"));
	}
}

BuiltinFunctionForEVM const* EVMDialectTyped::equalityFunction(YulName _type) const
{
	if (_type == boolType)
		return nullptr;
	else
	{
		yulAssert(_type == defaultType, "");
		return builtin(YulNameRegistry::instance().idOf("eq"));
	}
}

EVMDialectTyped const& EVMDialectTyped::instance(langutil::EVMVersion _version)
{
	static std::map<langutil::EVMVersion, std::unique_ptr<EVMDialectTyped const>> dialects;
	static YulStringRepository::ResetCallback callback{[&] { dialects.clear(); }};
	if (!dialects[_version])
		dialects[_version] = std::make_unique<EVMDialectTyped>(_version, true);
	return *dialects[_version];
}
