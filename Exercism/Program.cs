using System;
using System.Collections;
using System.Collections.Generic;

namespace Exercism
{
    internal class Program
    {
        // Caesar Cipher
        private static string Rotate(string text, int shiftKey)
        {
            char Cipher(char ch, int key)
            {
                if (!char.IsLetter(ch))
                    return ch;

                char d = char.IsUpper(ch) ? 'A' : 'a';
                return (char)(((ch + key - d) % 26) + d);
            }

            if (shiftKey % 26 == 0)
                return text;
            string res = "";
            foreach (var item in text)
                res += Cipher(item, shiftKey);

            return res;
        }
        // Matching Brackets
        public static bool IsPaired(string input)
        {
            Stack s = new Stack();
            Dictionary<char, char> corres = new Dictionary<char, char>()
            {
                { '(', ')' },
                { '{', '}' },
                { '[', ']' },
            };
            foreach (var item in input)
            {
                if (corres.ContainsKey(item))
                    s.Push(item);
                else if (corres.ContainsValue(item))
                {
                    if (corres[(char)s.Pop()] != item)
                        return false;
                }
                else
                    continue;
            }

            return s.Count == 0;
        }

        private static void Main(string[] args)
        {
            Console.WriteLine(Rotate("The quick brown fox jumps over the lazy dog.", 13));

            //Console.WriteLine(IsPaired("(){()[[]]}"));
            //Console.WriteLine(IsPaired("[({[{"));
            
            
            
            Console.ReadKey();
        }
    }
}