using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;

namespace HackerrankStacksQueues
{
    internal class Program
    {
        private static List<int[]> Util()
        {
            List<int[]> tests = new List<int[]>();
            using (StreamReader sr = new StreamReader("C:\\Users\\maxime.louward\\Downloads\\input.txt"))
            {
                sr.ReadLine(); // Ignores first line.
                while (!sr.EndOfStream)
                {
                    tests.Add(sr.ReadLine().Split(' ').ToList().ConvertAll(int.Parse).ToArray());
                }
            }
            return tests;
        }

        private static string IsBalanced(string s)
        {
            Dictionary<char, char> dict = new Dictionary<char, char>() {
                { '(',')' },
                { '{','}' },
                { '[',']' },
            };
            Stack<char> stack = new Stack<char>();
            foreach (char bracket in s)
            {
                if (dict.Keys.Contains(bracket)) stack.Push(bracket);
                else
                {
                    if (stack.Count != 0 && dict.TryGetValue(stack.Pop(), out char res))
                    {
                        if (res != bracket) return "NO";
                    }
                    else
                    {
                        return "NO";
                    }
                }
            }
            if (stack.Count == 0)
            {
                return "YES";
            }
            return "NO";
        }

        private static int EqualStacks(int[] h1, int[] h2, int[] h3)
        {
            Stack<int> s1 = new Stack<int>(h1.Length);
            Stack<int> s2 = new Stack<int>(h2.Length);
            Stack<int> s3 = new Stack<int>(h3.Length);
            for (int i = h1.Length - 1; i >= 0; i--)
            {
                s1.Push(h1[i]);
            }
            for (int i = h2.Length - 1; i >= 0; i--)
            {
                s2.Push(h2[i]);
            }
            for (int i = h3.Length - 1; i >= 0; i--)
            {
                s3.Push(h3[i]);
            }
            int t1 = s1.Sum(), t2 = s2.Sum(), t3 = s3.Sum();
            while (t1 != t2 || t2 != t3)
            {
                int n = new int[3] { t1, t2, t3 }.Max();
                if (n == t1)
                {
                    try
                    {
                        t1 -= s1.Pop();
                        continue;
                    }
                    catch (InvalidOperationException)
                    {
                        return 0;
                    }
                }
                if (n == t2)
                {
                    try
                    {
                        t2 -= s2.Pop();
                        continue;
                    }
                    catch (InvalidOperationException)
                    {
                        return 0;
                    }
                }
                if (n == t3)
                {
                    try
                    {
                        t3 -= s3.Pop();
                        continue;
                    }
                    catch (InvalidOperationException)
                    {
                        return 0;
                    }
                }
            }
            return t1;
        }

        private static int TwoStacks(int x, int[] a, int[] b)
        {
            int ai = 0;
            int bi = 0;
            int count;
            int sum = 0;
            while (bi < b.Length && sum + b[bi] <= x)
            {
                sum += b[bi];
                bi++;
            }
            count = bi;
            bi--; // loop exits only when bi reaches end or sum > x; in both case bi should decrease
            while (ai < a.Length && bi < b.Length)
            {
                sum += a[ai];
                if (sum > x)
                {
                    while (bi >= 0)
                    {
                        sum -= b[bi];
                        bi--;
                        if (sum <= x) break;
                    }
                    // if even no elements taken from B, but still sum greater than x, then a[ai] should not be chosen
                    // and loop terminates
                    if (sum > x && bi < 0)
                    {
                        ai--;
                        break;
                    }
                }
                count = Math.Max(ai + bi + 2, count);
                ai++;
            }
            return count;
        }

        private static void Main(string[] args)
        {
            var tests = Util();
            Stopwatch s = new Stopwatch();

            s.Start();
            Console.WriteLine(EqualStacks(tests[0], tests[1], tests[2]));
            s.Stop();

            Console.WriteLine(s.ElapsedMilliseconds + "ms");
            Console.ReadKey();
        }
    }
}