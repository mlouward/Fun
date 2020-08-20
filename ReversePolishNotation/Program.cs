using System;
using System.Collections;
using System.Globalization;

namespace ReversePolishNotation
{
    class Program
    {
        /// <summary>
        /// Computes calculation for RPN.
        /// </summary>
        /// <param name="operations"> A space separated string of the operations.</param>
        static double Evaluate(string operations)
        {
            Stack stack = new Stack(operations.Length);
            string[] op = operations.Split(' ');
            foreach (string c in op)
            {
                switch (c)
                {
                    case "+":
                        stack.Push(double.Parse(stack.Pop().ToString()) + double.Parse(stack.Pop().ToString()));
                        break;

                    case "-":
                        double a = double.Parse(stack.Pop().ToString());
                        double b = double.Parse(stack.Pop().ToString());
                        stack.Push(b - a);
                        break;

                    case "*":
                        stack.Push(double.Parse(stack.Pop().ToString()) * double.Parse(stack.Pop().ToString()));
                        break;

                    case "/":
                        double d = double.Parse(stack.Pop().ToString());
                        double e = double.Parse(stack.Pop().ToString());
                        stack.Push(e / d);
                        break;

                    // If c is an integer (not an operator).
                    default:
                        stack.Push(double.Parse(c, new CultureInfo("en-US")));
                        break;
                }
            }
            return double.Parse(stack.Pop().ToString());
        }
        static void Main(string[] args)
        {
            // 1 - (10 + 2) = -11
            Console.WriteLine(Evaluate("1 2 10 + -"));
            // (10 + 7.4 * 1.1) / ((10 + 4 * 1.1) - 14)
            Console.WriteLine(Evaluate("10 7.4 1.1 * + 10 4 1.1 * + 14 - /"));
            Console.ReadKey();
        }
    }
}
