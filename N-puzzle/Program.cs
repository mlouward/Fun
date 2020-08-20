using System;

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
        private static void Main(string[] args)
        {
            string test = StringToBitmask("1 3 6 4 9 5 2 8 7");
            Board b = new Board(test);
            Console.WriteLine(b);
            Console.WriteLine(b.Fitness());
            Console.ReadKey();
        }
    }
}