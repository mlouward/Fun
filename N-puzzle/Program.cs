using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Net.Security;

namespace N_puzzle
{
    internal class Program
    {
        private static string StringToBitmask(string s)
        {
            string res = "";
            var tab = s.Split(' ');
            foreach (string item in tab)
            {
                res += Convert.ToString(int.Parse(item), 2).PadLeft(5, '0');
            }
            return res;
        }

        private static void Solve(string test)
        {
            Board b = new Board(test);
            Console.WriteLine(b);
            b.SelectChildren();
        }

        private static void Main(string[] args)
        {
            Solve(StringToBitmask("1 3 6 4 9 5 2 8 7"));
            Console.ReadKey();
        }
    }
}