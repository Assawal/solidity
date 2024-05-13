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

#pragma once

#include <libyul/Dialect.h>

#include <libyul/backends/evm/AbstractAssembly.h>
#include <libyul/ASTForward.h>
#include <liblangutil/EVMVersion.h>

#include <map>
#include <set>

namespace solidity::yul
{

using Type = YulName;
struct FunctionCall;
struct Object;

/**
 * Context used during code generation.
 */
struct BuiltinContext
{
	Object const* currentObject = nullptr;
	/// Mapping from named objects to abstract assembly sub IDs.
	std::map<YulName, AbstractAssembly::SubID> subIDs;
};

struct BuiltinFunctionForEVM: public BuiltinFunction
{
	std::optional<evmasm::Instruction> instruction;
	/// Function to generate code for the given function call and append it to the abstract
	/// assembly. Expects all non-literal arguments of the call to be on stack in reverse order
	/// (i.e. right-most argument pushed first).
	/// Expects the caller to set the source location.
	std::function<void(FunctionCall const&, AbstractAssembly&, BuiltinContext&)> generateCode;
};


/**
 * Yul dialect for EVM as a backend.
 * The main difference is that the builtin functions take an AbstractAssembly for the
 * code generation.
 */
struct EVMDialect: public Dialect
{
	/// Constructor, should only be used internally. Use the factory functions below.
	EVMDialect(langutil::EVMVersion _evmVersion, bool _objectAccess);

	/// @returns the builtin function of the given name or a nullptr if it is not a builtin function.
	BuiltinFunctionForEVM const* builtin(YulName _name) const override;

	/// @returns true if the identifier is reserved. This includes the builtins too.
	bool reservedIdentifier(YulName _name) const override;

	bool reservedIdentifier(std::string_view const _label) const override;

	BuiltinFunctionForEVM const* discardFunction(YulName /*_type*/) const override { return builtin(YulNameRegistry::instance().idOf("pop")); }
	BuiltinFunctionForEVM const* equalityFunction(YulName /*_type*/) const override { return builtin(YulNameRegistry::instance().idOf("eq")); }
	BuiltinFunctionForEVM const* booleanNegationFunction() const override { return builtin(YulNameRegistry::instance().idOf("iszero")); }
	BuiltinFunctionForEVM const* memoryStoreFunction(YulName /*_type*/) const override { return builtin(YulNameRegistry::instance().idOf("mstore")); }
	BuiltinFunctionForEVM const* memoryLoadFunction(YulName /*_type*/) const override { return builtin(YulNameRegistry::instance().idOf("mload")); }
	BuiltinFunctionForEVM const* storageStoreFunction(YulName /*_type*/) const override { return builtin(YulNameRegistry::instance().idOf("sstore")); }
	BuiltinFunctionForEVM const* storageLoadFunction(YulName /*_type*/) const override { return builtin(YulNameRegistry::instance().idOf("sload")); }
	YulName hashFunction(YulName /*_type*/) const override { return YulNameRegistry::instance().idOf("keccak256"); }

	static EVMDialect const& strictAssemblyForEVM(langutil::EVMVersion _version);
	static EVMDialect const& strictAssemblyForEVMObjects(langutil::EVMVersion _version);

	langutil::EVMVersion evmVersion() const { return m_evmVersion; }

	bool providesObjectAccess() const { return m_objectAccess; }

	static SideEffects sideEffectsOfInstruction(evmasm::Instruction _instruction);

protected:
	BuiltinFunctionForEVM const* verbatimFunction(size_t _arguments, size_t _returnVariables) const;

	bool const m_objectAccess;
	langutil::EVMVersion const m_evmVersion;
	std::map<YulName, BuiltinFunctionForEVM> m_functions;
	std::map<std::pair<size_t, size_t>, std::shared_ptr<BuiltinFunctionForEVM const>> mutable m_verbatimFunctions;
	std::set<YulNameLabel, std::less<>> m_reservedLabels;
	std::set<YulName> m_reserved;
};

/**
 * EVM dialect with types u256 (default) and bool.
 * Difference to EVMDialect:
 *  - All comparison functions return type bool
 *  - bitwise operations are called bitor, bitand, bitxor and bitnot
 *  - and, or, xor take bool and return bool
 *  - iszero is replaced by not, which takes bool and returns bool
 *  - there are conversion functions bool_to_u256 and u256_to_bool.
 *  - there is popbool
 */
struct EVMDialectTyped: public EVMDialect
{
	/// Constructor, should only be used internally. Use the factory function below.
	EVMDialectTyped(langutil::EVMVersion label, bool _objectAccess);

	BuiltinFunctionForEVM const* discardFunction(YulName _type) const override;
	BuiltinFunctionForEVM const* equalityFunction(YulName _type) const override;
	BuiltinFunctionForEVM const* booleanNegationFunction() const override { return builtin(YulNameRegistry::instance().reserved().instruction_not); }

	static EVMDialectTyped const& instance(langutil::EVMVersion _version);

	// type for popbool
	YulName popboolType;
};

}
