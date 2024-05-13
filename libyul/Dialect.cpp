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
 * Yul dialect.
 */

#include <libyul/Dialect.h>
#include <libyul/AST.h>

using namespace solidity::yul;
using namespace solidity::langutil;

Literal Dialect::zeroLiteralForType(YulName _type) const
{
	auto const& registry = YulNameRegistry::instance();
	if (_type == boolType && _type != defaultType)
		return {DebugData::create(), LiteralKind::Boolean, registry.reserved().bfalse, _type};
	return {DebugData::create(), LiteralKind::Number, registry.reserved().ui0, _type};
}


Literal Dialect::trueLiteral() const
{
	auto const& registry = YulNameRegistry::instance();
	if (boolType != defaultType)
		return {DebugData::create(), LiteralKind::Boolean, registry.reserved().btrue, boolType};
	else
		return {DebugData::create(), LiteralKind::Number, registry.reserved().ui1, defaultType};
}

bool Dialect::validTypeForLiteral(LiteralKind _kind, YulName, YulName _type) const
{
	if (_kind == LiteralKind::Boolean)
		return _type == boolType;
	else
		return true;
}

Dialect const& Dialect::yulDeprecated()
{
	static std::unique_ptr<Dialect> dialect;
	static YulStringRepository::ResetCallback callback{[&] { dialect.reset(); }};
	auto& registry = YulNameRegistry::instance();
	if (!dialect)
	{
		// TODO will probably change, especially the list of types.
		dialect = std::make_unique<Dialect>();
		dialect->defaultType = registry.idOf("u256");
		dialect->boolType = registry.idOf("bool");
		dialect->types = {registry.idOf("bool"),
			   registry.idOf("u8"),
			   registry.idOf("s8"),
			   registry.idOf("u32"),
			   registry.idOf("s32"),
			   registry.idOf("u64"),
			   registry.idOf("s64"),
			   registry.idOf("u128"),
			   registry.idOf("s128"),
			   registry.idOf("u256"),
			   registry.idOf("s256")
		};
	};

	return *dialect;
}
