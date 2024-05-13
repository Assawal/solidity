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
 * Definition of the YulName, an identifier for Handles.
 */

#pragma once

#include <liblangutil/Token.h>

#include <vector>
#include <map>
#include <set>
#include <cstdint>
#include <string_view>
#include <optional>
#include <sstream>
#include <algorithm>

namespace solidity::yul
{

using YulName = std::uint64_t;
using YulNameLabel = std::string;

struct ReservedYulNames
{
	YulName bfalse;
	YulName btrue;
	YulName ui0;
	YulName ui1;
	YulName ui32;

	YulName selfdestruct;
	YulName tstore;
	YulName datasize;
	YulName dataoffset;

	YulName instruction_not;
	YulName instruction_shl;
	YulName instruction_exp;
	YulName instruction_mul;
	YulName instruction_add;
	YulName instruction_sub;

	YulName eq;
	YulName memoryguard;
};

class YulNameRegistry
{
public:

	static YulNameRegistry& instance()
	{
		static YulNameRegistry inst;
		return inst;
	}

	YulNameRegistry(YulNameRegistry const&) = delete;
	YulNameRegistry& operator=(YulNameRegistry const&) = delete;
	YulNameRegistry(YulNameRegistry &&) = delete;
	YulNameRegistry& operator=(YulNameRegistry &&) = delete;

	static constexpr bool empty(YulName const id) {
		return id == 0;
	}

	static constexpr bool empty(std::string_view const label) {
		return label.empty();
	}

	static constexpr YulName emptyId() {
		return 0;
	}

	std::string_view resolve(YulName id) const {
		return labelFor(id);
	}

	YulNameLabel const& resolve_s(YulName id) const {
		return labelFor(id);
	}

	YulName add(YulName parent) {
		m_ids.push_back(parent);
		return m_ids.size() - 1;
	}

	YulName idOf(std::string_view label) {
		for(std::size_t i = 0; i < m_labels.size(); ++i)
			if(m_labels[i] == label)
				return i;

		m_ids.push_back(m_labels.size());
		m_labels.emplace_back(label);
		return m_ids.size() - 1;
	}

	ReservedYulNames const& reserved() const {
		return m_reserved;
	}

	bool isYulKeyword(YulName const name) const {
		return m_yulKeywords.count(name) > 0;
	}

	bool isYulKeyword(std::string_view const label) const {
		return m_yulKeywordLabels.count(label) > 0;
	}

	bool used(std::string_view const label) const {
		if(std::find(m_labels.begin(), m_labels.end(), label) != m_labels.end()) {
			return true;
		}
		auto const isDerivedName = std::any_of(
			m_derivedLabelsCache.begin(), m_derivedLabelsCache.end(),
			[label](auto const& item) { return item.second == label; });
		if(isDerivedName)
			return true;
		return false;
	}
private:
	YulNameRegistry() {
		m_reserved.bfalse = idOf("false");
		m_reserved.btrue = idOf("true");
		m_reserved.ui0 = idOf("0");
		m_reserved.ui1 = idOf("1");
		m_reserved.ui32 = idOf("32");

		m_reserved.selfdestruct = idOf("selfdestruct");
		m_reserved.tstore = idOf("tstore");
		m_reserved.datasize = idOf("datasize");
		m_reserved.dataoffset = idOf("dataoffset");
		m_reserved.instruction_not = idOf("not");
		m_reserved.instruction_shl = idOf("shl");
		m_reserved.instruction_exp = idOf("exp");
		m_reserved.instruction_mul = idOf("mul");
		m_reserved.instruction_add = idOf("add");
		m_reserved.instruction_sub = idOf("sub");
		m_reserved.eq = idOf("eq");
		m_reserved.memoryguard = idOf("memoryguard");

		for(auto const&[k, _] : langutil::TokenTraits::yulKeywordsByName())
			m_yulKeywords.emplace(idOf(k));
		m_yulKeywords.emplace(idOf("leave"));
	}

	YulNameLabel deriveLabel(YulName id) const {
		id = m_ids[id];
		std::stringstream postfix;
		while(id >= m_labels.size()) {
			postfix << "_" << id;
			id = m_ids[id];
		}
		auto const targetName = m_labels[id] + postfix.str();
		auto substituteName = targetName;
		size_t postfixInc = 0;
		while(used(substituteName)) {
			substituteName = targetName + "_" + std::to_string(postfixInc);
			++postfixInc;
		}
		return substituteName;
	}

	YulNameLabel const& labelFor(YulName id) const {
		if(m_ids[id] < m_labels.size()) {
			return m_labels[m_ids[id]];
		} else {
			auto [it, inserted] = m_derivedLabelsCache.try_emplace(id);
			if(inserted)
				it->second = deriveLabel(id);
			return it->second;
		}
	}

	std::vector<YulName> m_ids {0};
	std::vector<YulNameLabel> m_labels {""};
	std::map<YulName, YulNameLabel> mutable m_derivedLabelsCache {};
	std::set<YulNameLabel, std::less<>> m_yulKeywordLabels{};
	std::set<YulName> m_yulKeywords{};

	ReservedYulNames m_reserved{};
};

}
