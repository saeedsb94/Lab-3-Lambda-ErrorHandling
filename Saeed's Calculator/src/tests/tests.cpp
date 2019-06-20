#include "mathLib.hpp"
#include "gtest/gtest.h"

/*	Case 1:	Precedence	*/
TEST(Calculator, Precedence) {

	EXPECT_TRUE(Saeed_Calculator::Compare("25 + 5  -  6 / 3 + 0.5",28.5));
}

/*	Case 2:	brackets	*/
TEST(Calculator, brackets) {

	EXPECT_TRUE(Saeed_Calculator::Compare("((-6(2/2+1))+7)", -5));
}

/*	Case 3:	Left To Right	*/
TEST(Calculator, Left_To_Right) {

	EXPECT_TRUE(Saeed_Calculator::Compare("5-3-1+7", 8));
}

/*	Case 4: Right To Left	*/
TEST(Calculator, Right_To_Left) {

	EXPECT_TRUE(Saeed_Calculator::Compare("sqrt(10^3^2)", 31622.776601683792));
}

/*	Case 5:	DIVIDE_BY_ZERO	*/

int main(int argc, char* argv[])

{
	::testing::InitGoogleTest(&argc, argv);
	return RUN_ALL_TESTS();

}
