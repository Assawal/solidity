//
// Created by mho on 5/8/24.
//

#include <libyul/YulName.h>
#include <boost/test/unit_test.hpp>

using namespace solidity::yul;

BOOST_AUTO_TEST_CASE(YulNameTest) {
	YulNameRegistry resolver;

	auto const idA = resolver.idOf("a");
	auto const idB = resolver.idOf("b");
	auto const idDependsA = resolver.add(idA);

	auto yulNameDependsA = resolver.resolve(idDependsA);
	auto yulNameB = resolver.resolve(idB);

	BOOST_CHECK_EQUAL("b", yulNameB);
	BOOST_CHECK_EQUAL("a_2", yulNameDependsA);
}
