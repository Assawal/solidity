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
 * Yul code and data object container.
 */

#include <libyul/Object.h>

#include <libyul/AsmPrinter.h>
#include <libyul/AsmJsonConverter.h>
#include <libyul/AST.h>
#include <libyul/Exceptions.h>

#include <libsolutil/CommonData.h>
#include <libsolutil/StringUtils.h>

#include <boost/algorithm/string.hpp>

#include <range/v3/view/transform.hpp>

using namespace solidity;
using namespace solidity::langutil;
using namespace solidity::util;
using namespace solidity::yul;

std::string Data::toString(Dialect const*, DebugInfoSelection const&, CharStreamProvider const*) const
{
	return "data \"" + YulNameRegistry::instance().resolve_s(name) + "\" hex\"" + util::toHex(data) + "\"";
}

std::string Object::toString(
	Dialect const* _dialect,
	DebugInfoSelection const& _debugInfoSelection,
	CharStreamProvider const* _soliditySourceProvider
) const
{
	yulAssert(code, "No code");
	yulAssert(debugData, "No debug data");

	std::string useSrcComment;

	if (debugData->sourceNames)
		useSrcComment =
			"/// @use-src " +
			joinHumanReadable(ranges::views::transform(*debugData->sourceNames, [](auto&& _pair) {
				return std::to_string(_pair.first) + ":" + util::escapeAndQuoteString(*_pair.second);
			})) +
			"\n";

	std::string inner = "code " + AsmPrinter(
		_dialect,
		debugData->sourceNames,
		_debugInfoSelection,
		_soliditySourceProvider
	)(*code);

	for (auto const& obj: subObjects)
		inner += "\n" + obj->toString(_dialect, _debugInfoSelection, _soliditySourceProvider);

	return useSrcComment + "object \"" + YulNameRegistry::instance().resolve_s(name) + "\" {\n" + indent(inner) + "\n}";
}

Json Data::toJson() const
{
	Json ret;
	ret["nodeType"] = "YulData";
	ret["value"] = util::toHex(data);
	return ret;
}

Json Object::toJson() const
{
	yulAssert(code, "No code");

	Json codeJson;
	codeJson["nodeType"] = "YulCode";
	codeJson["block"] = AsmJsonConverter(0 /* sourceIndex */)(*code);

	Json subObjectsJson = Json::array();
	for (std::shared_ptr<ObjectNode> const& subObject: subObjects)
		subObjectsJson.emplace_back(subObject->toJson());

	Json ret;
	ret["nodeType"] = "YulObject";
	ret["name"] = YulNameRegistry::instance().resolve_s(name);
	ret["code"] = codeJson;
	ret["subObjects"] = subObjectsJson;
	return ret;
}

std::set<YulName> Object::qualifiedDataNames() const
{
	auto &registry = YulNameRegistry::instance();
	std::set<YulName> qualifiedNames =
		registry.empty(name) || util::contains(registry.resolve_s(name), '.') ?
		std::set<YulName>{} :
		std::set<YulName>{name};
	for (std::shared_ptr<ObjectNode> const& subObjectNode: subObjects)
	{
		yulAssert(qualifiedNames.count(subObjectNode->name) == 0, "");
		if (util::contains(registry.resolve_s(subObjectNode->name), '.'))
			continue;
		qualifiedNames.insert(subObjectNode->name);
		if (auto const* subObject = dynamic_cast<Object const*>(subObjectNode.get()))
			for (YulName const& subSubObj: subObject->qualifiedDataNames())
				if (subObject->name != subSubObj)
				{
					auto const name = registry.resolve_s(subObject->name) + "." + registry.resolve_s(subSubObj);
					auto const id = registry.idOf(name);
					yulAssert(qualifiedNames.count(id) == 0, "");
					qualifiedNames.insert(id);
				}
	}

	yulAssert(qualifiedNames.count(YulName{}) == 0, "");
	qualifiedNames.erase(YulName{});
	return qualifiedNames;
}

std::vector<size_t> Object::pathToSubObject(YulName _qualifiedName) const
{
	auto &registry = YulNameRegistry::instance();
	yulAssert(_qualifiedName != name, "");
	yulAssert(subIndexByName.count(name) == 0, "");

	auto resolvedQualifiedName = registry.resolve_s(_qualifiedName);
	auto const& resolvedName = registry.resolve_s(name);
	if (boost::algorithm::starts_with(resolvedQualifiedName, resolvedName + "."))
	{
		resolvedQualifiedName = resolvedQualifiedName.substr(resolvedName.length() + 1);
		_qualifiedName = registry.idOf(resolvedQualifiedName);
	}
	yulAssert(!registry.empty(_qualifiedName), "");

	std::vector<std::string> subObjectPathComponents;
	boost::algorithm::split(subObjectPathComponents, resolvedQualifiedName, boost::is_any_of("."));

	std::vector<size_t> path;
	Object const* object = this;
	for (std::string const& currentSubObjectName: subObjectPathComponents)
	{
		yulAssert(!currentSubObjectName.empty(), "");
		auto subIndexIt = object->subIndexByName.find(registry.idOf(currentSubObjectName));
		yulAssert(
			subIndexIt != object->subIndexByName.end(),
			"Assembly object <" + resolvedQualifiedName + "> not found or does not contain code."
		);
		object = dynamic_cast<Object const*>(object->subObjects[subIndexIt->second].get());
		yulAssert(object, "Assembly object <" + resolvedQualifiedName + "> not found or does not contain code.");
		yulAssert(object->subId != std::numeric_limits<size_t>::max(), "");
		path.push_back({object->subId});
	}

	return path;
}
