
#include "mathLib.hpp"
#include <stdexcept>
#include <sstream>
#include <iostream>
#include <stack>
#include <cstddef>
#include <cctype>
#include <vector>
#include <cmath>
#include <string>
#include <iostream>
namespace Saeed_Calculator
{
	class error : public std::runtime_error
	{
	public:
		error(const std::string& expr, const std::string& message)
			: std::runtime_error(message),
			expr_(expr)
		{ }
#if __cplusplus < 201103L
		~error() throw() { }
#endif
		std::string expression() const
		{
			return expr_;
		}
	private:
		std::string expr_;
	};

	struct Operator
	{
		///	Operator, one of the OPERATOR_* enum definitions
		int type;
		///	Operator precedence
		int precedence;
		///	'L' = left to right priority or 'R' = right to left priority
		bool associativity;
		Operator(int opr, int prec, int assoc) :
			type(opr)
		{}
	};

	template <typename T>
	class SingleOperation
	{
		Operator mOperator;
		std::vector<T> mArguments;
		T mResult;

	public:
		SingleOperation(const Operator& opr, const std::vector<T>& args) :
			currentOperation(opr), arguments(args)
		{
			
		}
		~SingleOperation()
		{
			arguments.clear;
		}
		T getOprResult() {
			Calculate();
			return mResult;
		}
	private:
		//	Avilable Operator
		enum
		{
			OPERATOR_NULL,
			OPERATOR_ADDITION,       /// +
			OPERATOR_SUBTRACTION,    /// -
			OPERATOR_MULTIPLICATION, /// *
			OPERATOR_DIVISION,       /// /
			OPERATOR_MODULO,         /// % || mod
			OPERATOR_POWER,          /// ^
			OPERATOR_ROOT,          /// sqrt
			OPERATOR_INVERSE,          /// inv 1/X
			OPERATOR_MEMORY,          /// Access Memory MR
		};

		T Calculate() const
		{
			switch (currentOperation.type)
			{
			case OPERATOR_BITWISE_OR:
				return arguments[0] | arguments[1];
			case OPERATOR_BITWISE_XOR:
				return arguments[0] ^ arguments[1];
			case OPERATOR_BITWISE_AND:
				return arguments[0] & arguments[1];
			case OPERATOR_BITWISE_SHL:
				return arguments[0] << arguments[1];
			case OPERATOR_BITWISE_SHR:
				return arguments[0] >> arguments[1];
			case OPERATOR_ADDITION:
				return arguments[0] + arguments[1];
			case OPERATOR_SUBTRACTION:
				return arguments[0] - arguments[1];
			case OPERATOR_MULTIPLICATION:
				return arguments[0] * arguments[1];
			case	OPERATOR_INVERSE:	          /// inv
				return 1 / arguments[0];
			case		OPERATOR_MEMORY:          /// inv
				return mMemory[arguments[0]];
				OPERATOR_EXPONENT        /// e, E
			case OPERATOR_DIVISION:					return v1 / checkZero(v2);
			case OPERATOR_MODULO:				return (int)v1 % (int)checkZero(v2);
			default:                      return 0;
			}
		}




	};


	template <typename T>
	class ExpressionParser
	{
		/// Expression string
		std::string mExpression;

		/// Current expression index, incremented whilst parsing
		std::size_t mExpressionIndex;

		/// The current operator and its left value are pushed onto the stack if the operator on top of the stack has lower precedence.
		std::stack<SingleOperation<T>> mOperatorStack;

		/// the result of the current expression
		T mAns;

		///	Stored memory MR[n]
		std::vector<T> mMemory;

	public:
		//	Constructor ExpressionParser
		ExpressionParser()
			: mAns(0), mExpression(""), mExpressionIndex(0)
		{}
		/// Evaluate an  arithmetic expression
		/// @throw error if parsing fails.
		void Evaluate(const std::string& expression)
		{
			T result = 0;
			mExpressionIndex = 0;
			mExpression = expression;
			try
			{
				result = parseExpr();
				if (!isEnd())
					unexpected();
			}
			catch (const Saeed_Calculator::error&)
			{
				while (!stack_.empty())
					stack_.pop();
				throw;
			}
			return result;
		}
		//returns the result of the last evaluated expression
		T GetAns() const
		{
			return mAns;
		}
		~ExpressionParser()
		{
			mMemory.clear();
		}

	private:




		bool IsExpressionEnd() const
		{
			return index_ >= expr_.size();
		}

		/// Returns the character at the current expression index or
		/// 0 if the end of the expression is reached.
		char getCharacter() const
		{
			if (!IsExpressionEnd())
				return mExpression[mExpressionIndex];
			return 0;
		}

		/// Parse str at the current expression index.
		/// @throw error if parsing fails.
		///
		void expect(const std::string& str)
		{
			if (expr_.compare(mExpressionIndex, str.size(), str) != 0)
				unexpected();
			mExpressionIndex += str.size();
		}

		void unexpected() const
		{
			std::ostringstream msg;
			msg << "Syntax error: unexpected token \""
				<< expr_.substr(index_, expr_.size() - index_)
				<< "\" at index "
				<< mExpressionIndex;
			throw Saeed_Calculator::error(expr_, msg.str());
		}

		/// Eat all white space characters at the
		/// current expression index.
		///
		void eatSpaces()
		{
			while (std::isspace(getCharacter()) != 0)
				mExpressionIndex++;
		}

		/// Parse a binary operator at the current expression index.
		/// @return Operator with precedence and associativity.
		///
		Operator parseOp()
		{
			switch (getCharacter())
			{
			case '|': index_++;     return Operator(OPERATOR_BITWISE_OR, 4, 'L');
			case '&': index_++;     return Operator(OPERATOR_BITWISE_AND, 6, 'L');
			case '<': expect("<<"); return Operator(OPERATOR_BITWISE_SHL, 9, 'L');
			case '>': expect(">>"); return Operator(OPERATOR_BITWISE_SHR, 9, 'L');
			case '+': index_++;     return Operator(OPERATOR_ADDITION, 10, 'L');
			case '-': index_++;     return Operator(OPERATOR_SUBTRACTION, 10, 'L');
			case '/': index_++;     return Operator(OPERATOR_DIVISION, 20, 'L');
			case '%': index_++;     return Operator(OPERATOR_MODULO, 20, 'L');
			case '*': index_++; if (getCharacter() != '*')
				return Operator(OPERATOR_MULTIPLICATION, 20, 'L');
				index_++;     return Operator(OPERATOR_POWER, 30, 'R');
			case '^': index_++;     return Operator(OPERATOR_POWER, 30, 'R');
			case 'e': index_++;     return Operator(OPERATOR_EXPONENT, 40, 'R');
			case 'E': index_++;     return Operator(OPERATOR_EXPONENT, 40, 'R');
			default:               return Operator(OPERATOR_NULL, 0, 'L');
			}
		}
		// Convert the Char to digit
		T getDigit() const
		{
				return getCharacter() - '0';
		}
		bool IsDigit(T d) {
			return d <= 9 || d >= 0;
		}

		T getFraction(T d) {
			mExpressionIndex++; //read the '.' sympol
			T value = 0;
			T d = getDigit();
			if (!IsDigit(d))
				unexpected();
			T coeff = 1;
			while (IsDigit(d)) {
				coeff /= 10;
				value = value + d * coeff;
				mExpressionIndex++;
				d = getDigit();
			}

			return value;
		}

		T getWhole_Number() 
		{
			T value = 0;
			T d = getDigit();
			while (IsDigit(d))
			{				
				value = value * 10 + d;
				mExpressionIndex++;
				d = getDigit();
			}
			return value;
		}
		
	

	T parseNumber()
	{
		T value = 0;
		value += getWhole_Number();
		if (getCharacter() == '.')
			value += getFraction();
		return value;
	}


	/// Parse an integer value at the current expression index.
	/// The unary `+', `-' and `~' operators and opening
	/// parentheses `(' cause recursion.
	///
	SingleOperation<T> getOperation()
	{
		Operator opr(OPERATOR_NULL);
		std::vector<T> args;
		switch (getCharacter())
		{
		case '0': case '1': case '2': case '3': case '4': case '5':
		case '6': case '7': case '8': case '9': case '.':
			args.push_back(parseNumber());
			break;
		case '(':
			mExpressionIndex++;
			val = parseExpr();
			eatSpaces();
			if (getCharacter() != ')')
			{
				if (!isEnd())
					unexpected();
				throw Saeed_Calculator::error(expr_, "Syntax error: `)' expected at end of expression");
			}
			index_++;
			break;
		case '~': index_++; val = ~parseValue(); break;
		case '+': index_++; val = parseValue(); break;
		case '-': index_++; val = parseValue() * static_cast<T>(-1);
			break;
		default: if (!isEnd())
			unexpected();
			throw Saeed_Calculator::error(expr_, "Syntax error: value expected at end of expression");
		}
		return SingleOperation<T>(op, args);
	}

	/// Parse all operations of the current parenthesis
	/// level and the levels above, when done
	/// return the result (value).
	///


	T parseExpr()
	{

		SingleOperation<T> opr = getOperation();

		mOperatorStack.push(SingleOperation<T>(Operator(OPERATOR_NULL), std::vector<T>(opr.getOprResult()));

		while (!mOperatorStack.empty())
		{
			// parse an operator (+, -, *, ...)
			Operator op(parseOp());
			while (op.precedence < stack_.top().getPrecedence() || (
				op.precedence == stack_.top().getPrecedence() &&
				op.associativity == 'L'))
			{
				// end reached
				if (stack_.top().isNull())
				{
					stack_.pop();
					return value;
				}
				// do the calculation ("reduce"), producing a new value
				value = calculate(stack_.top().value, value, stack_.top().op);
				stack_.pop();
			}

			// store on stack_ and continue parsing ("shift")
			stack_.push(OperatorValue(op, value));
			// parse value on the right
			value = parseValue();
		}
		return 0;
	}
	T checkZero(T value) const
	{
		if (value == 0)
		{
			std::string divOperators("/%");
			std::size_t division = mExpressionIndex.find_last_of(divOperators, mExpressionIndex - 2);
			std::ostringstream msg;
			msg << "Parser error: division by 0";
			if (division != std::string::npos)
				msg << " (error token is \""
				<< mExpressionIndex.substr(division, mExpressionIndex.size() - division)
				<< "\")";
			throw Saeed_Calculator::error(mExpressionIndex, msg.str());
		}
		return value;
	}

};







void RunCalculator()
{
	try
	{
		std::string buffer;
		ExpressionParser <double> parser;
		std::cout << "Enter an expression to evaluate, or an empty line to quit." << std::endl;
		while (std::getline(std::cin, buffer)) {
			if (buffer[0] == '\0')
				break;
			parser.Evaluate(buffer);
			std::cout << parser.GetAns() << std::endl;
		}
	}
	catch (Saeed_Calculator::error & e)
	{
		std::cerr << e.what() << std::endl;
	}
}

}

