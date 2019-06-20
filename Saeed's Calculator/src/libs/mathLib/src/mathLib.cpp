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

		std::string expression() const
		{
			return expr_;
		}
	private:
		std::string expr_;
	};


	class ExpressionParser {
	private:

		std::string mExpr;
		int mPos;
		double mRes;
		std::vector<double> mMemory;

	public:
		ExpressionParser(std::string expr)
			: mExpr(expr), mPos(0)
		{
			PreProcessing();
			mRes = evalute();
			mMemory.push_back(0);
		}
		~ExpressionParser()
		{

		}
		void Parse(std::string expr) {
			mExpr = expr;
			mPos = 0;
			PreProcessing();
			mRes = evalute();
			mMemory[0] = mRes;
		}
		double GetAns() {
			return mRes;
		}

	private:

		//Operators Coding
		enum
		{
			OPERATOR_ADDITION,       /// +
			OPERATOR_SUBTRACTION,    /// -
			OPERATOR_MULTIPLICATION, /// *
			OPERATOR_DIVISION,       /// /
			OPERATOR_MODULO,         /// %
			OPERATOR_POWER,          /// ^
			OPERATOR_ROOT,          /// sqrt
			OPERATOR_INVERSE,       /// 1/x
			OPERATOR_READ_MEMORY,       /// MR
			OPERATOR_WRITE_MEMORY,       ///MW
		};

		//Operator' precedence Coding
		enum
		{
			LEVEL_0,	/// '+'	'-'
			LEVEL_1,    ///	'*'	'/'	'%'
			LEVEL_2,	/// '^'	'sqrt'	'1/x'
			LEVEL_3,	/// 'MR'	'MW'
			LEVEL_4,	/// '()'
		};

		struct Operator
		{
			int op;
			int precedence;
			char associativity;
			int numParameters;
			Operator(int opr, int prec, char assoc, int numPar) :
				op(opr),
				precedence(prec),
				associativity(assoc),
				numParameters(numPar)
			{ }
		};

		void PreProcessing() {
			for (int i = 0; i < mExpr.size(); i++)
			{
				mExpr[i] = tolower(mExpr[i]);
				if (mExpr[i] == ' ') {
					mExpr.erase(i, 1);
					i--;
				}
			}
			for (int i = 0; i < mExpr.size(); i++)
			{
				if (mExpr[i] == '(' && i != 0 && IsDigit(mExpr[i - 1])) {
					mExpr.insert(i, "*");
				}
			}

		}

		void unexpected() const
		{
			std::ostringstream msg;
			msg << "Equation after PreProcessing looks like :   "
				<< mExpr << "\n"
				<< "Syntax error: unexpected token \""
				<< mExpr.substr(mPos, mExpr.size() - mPos)
				<< "\" at index "
				<< mPos;
			throw Saeed_Calculator::error(mExpr, msg.str());
		}

		void expect(const std::string& str)
		{
			if (mExpr.compare(mPos, str.size(), str) != 0)
				unexpected();
			mPos += str.size();
		}

		char getCharacter() {
			return mExpr[mPos];
		}

		bool IsDigit(char c) {
			return c <= '9' && c >= '0';
		}

		// Convert the Char to digit
		short getDigit()
		{
			return getCharacter() - '0';
		}

		double getFraction() {
			mPos++; //read the '.' sympol
			double value = 0;
			if (!IsDigit(getCharacter()))
				unexpected();
			double coeff = 1;
			while (IsDigit(getCharacter())) {
				double	d = getDigit();
				coeff /= 10;
				value = value + d * coeff;
				mPos++;
			}
			return value;
		}

		double getWhole_Number()
		{
			double value = 0.0;
			while (IsDigit(getCharacter()))
			{
				short d = getDigit();
				value = value * 10 + d;
				mPos++;

			}
			return value;
		}

		double parseNumber()
		{
			double value = 0.0;
			value += getWhole_Number();
			if (getCharacter() == '.')
				value += getFraction();
			return value;
		}

		double checkZero(double value) const
		{
			if (value == 0)
			{
				std::string divOperators("/%");
				std::size_t division = mExpr.find_last_of(divOperators, mPos - 2);
				std::ostringstream msg;
				msg << "Parser error: division by 0";
				if (division != std::string::npos)
					msg << " (error token is \""
					<< mExpr.substr(division, mExpr.size() - division)
					<< "\")";
				throw Saeed_Calculator::error(mExpr, msg.str());
			}
			return value;
		}

		Operator parseOperation() {
			//Get Operation or unexpexted
			switch (getCharacter())
			{
			case '+':
				mPos++;	return Operator(OPERATOR_ADDITION, LEVEL_1, 'L', 2);
			case '-':
				mPos++;	return Operator(OPERATOR_SUBTRACTION, LEVEL_1, 'L', 2);
			case '/':
				mPos++;	return Operator(OPERATOR_DIVISION, LEVEL_2, 'L', 2);
			case '*':
				mPos++;	return Operator(OPERATOR_MULTIPLICATION, LEVEL_2, 'L', 2);
			case '%':
				mPos++;	return Operator(OPERATOR_MODULO, LEVEL_2, 'L', 2);
			case '^':
				mPos++;	return Operator(OPERATOR_POWER, LEVEL_3, 'R', 2);
			case '~':
				expect("~(");	mPos--;	return Operator(OPERATOR_INVERSE, LEVEL_3, 'R', 1);
			case 's':
				expect("sqrt(");	mPos--;	return Operator(OPERATOR_ROOT, LEVEL_3, 'R', 1);
			case 'm':
				if ((mPos + 1) == mExpr.length())
				{
					mPos--;
					unexpected();
				}
				else {
					if (mExpr[mPos + 1] == 'r') {

						expect("mr");
						return Operator(OPERATOR_READ_MEMORY, LEVEL_4, 'R', 1);
					}
					else if (mExpr[mPos + 1] == 'w') {
						expect("mw");
						return Operator(OPERATOR_WRITE_MEMORY, LEVEL_4, 'R', 1);
					}
					else
						unexpected();

				}

			default:               unexpected();
			}
		}

		double calculate(double& v1, double& v2, const Operator& op)
		{
			switch (op.op)
			{
			case OPERATOR_ADDITION:       return v1 + v2;
			case OPERATOR_SUBTRACTION:    return v1 - v2;
			case OPERATOR_MULTIPLICATION: return v1 * v2;
			case OPERATOR_DIVISION:       return v1 / checkZero(v2);
			case OPERATOR_MODULO:         return (int)((int)v1 % (int)checkZero(v2));
			case OPERATOR_POWER:          return pow(v1, v2);
			case OPERATOR_ROOT:          return sqrt(v1);
			case OPERATOR_INVERSE:          return 1 / v1;
			case OPERATOR_READ_MEMORY:
				if (v1 >= mMemory.size() || v1 == 0)
				{
					std::ostringstream msg;
					msg << "Parser error: Invalid INDEX";
					throw Saeed_Calculator::error(mExpr, msg.str());
				}
				return mMemory[(int)v1];
			case OPERATOR_WRITE_MEMORY:
				if (v1 > mMemory.size() || v1 == 0)
				{
					std::ostringstream msg;
					msg << "Parser error: Invalid INDEX";
					throw Saeed_Calculator::error(mExpr, msg.str());
				}
				if (v1 == mMemory.size())
					mMemory.push_back(mMemory[0]);
				else
					mMemory[(int)v1] = mMemory[0];

				return 0.0;

			default:                      return 0.0;
			}
		}


		void terminate(std::stack<double>& parameters, std::stack<Operator>& operations) {
			double val = 0.0;
			while (!operations.empty()) {
				//calculate
				Operator op = operations.top();
				double v1 = 0.0;
				double v2 = 0.0;
				operations.pop();
				if (op.numParameters > parameters.size())
				{
					mPos--;
					unexpected();
				}
				if (op.numParameters == 2) {
					v2 = parameters.top();
					parameters.pop();
					v1 = parameters.top();
					parameters.pop();
				}
				else
				{
					v1 = parameters.top();
					parameters.pop();
				}

				if (op.op == OPERATOR_WRITE_MEMORY && parameters.size() > 1)
				{
					unexpected();
				}
				parameters.push(calculate(v1, v2, op));

			}


		}

		double evalute() {

			std::stack<double> parameters;
			std::stack<Operator> operations;




			while (mPos < mExpr.length())
			{
				char c = mExpr[mPos];

				if (c == '(') {
					mPos++;
					parameters.push(evalute());
					//ADD Numbers	parseNumber()
				}
				else if (c == ')' || c == ']')
				{
					mPos++;
					break;
				}
				else if (IsDigit(c))
				{
					parameters.push(parseNumber());
					//ADD Numbers	parseNumber()
				}
				else
				{

					Operator op = parseOperation();

					// Proccess the precedance
					if ((!operations.empty()) && (op.precedence < operations.top().precedence || (op.precedence == operations.top().precedence && op.associativity == 'L')))
						terminate(parameters, operations); //calc the top
					//ADD Operation	parseOperation()
					if ((op.op == OPERATOR_SUBTRACTION || op.op == OPERATOR_ADDITION) && parameters.empty())
						parameters.push(0.0);

					if ((op.op == OPERATOR_WRITE_MEMORY || op.op == OPERATOR_READ_MEMORY))
					{
						if (mExpr[mPos] != '[') {
							parameters.push(1);
						}
						else
						{
							mPos++;
							double tmp = evalute();
							if (!(floor(tmp) == ceil(tmp)))
							{
								std::ostringstream msg;
								msg << "Parser error: index must be INTEGER";
								throw Saeed_Calculator::error(mExpr, msg.str());
							}
							else
							{
								parameters.push(tmp + 1);
							}
						}
					}


					operations.push(op);
				}

			}
			terminate(parameters, operations);

			if (parameters.size() != 1)
				unexpected();
			else
				return parameters.top();
		}
	};

	void RunCalculator()
	{
		try
		{
			std::string buffer = "0.0";
			ExpressionParser  parser(buffer);
			std::cout << "Enter an expression to evaluate, or an empty line to quit." << std::endl;
			while (std::getline(std::cin, buffer)) {
				if (buffer[0] == '\0')
					break;
				parser.Parse(buffer);
				std::cout << parser.GetAns() << std::endl;
			}
		}
		catch (Saeed_Calculator::error& e)
		{
			std::cerr << e.what() << std::endl;
		}
	}
	bool Compare(std::string buffer, double res)
	{

		Saeed_Calculator::ExpressionParser  parser(buffer);
		return parser.GetAns() == res;

	}
}



