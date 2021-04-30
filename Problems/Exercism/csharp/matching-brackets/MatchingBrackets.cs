using System;
using System.Collections;
using System.Collections.Generic;

public static class MatchingBrackets
{
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
                if (s.Count != 0 && corres[(char)s.Pop()] != item)
                    return false;
            }
        }

        return s.Count == 0;
    }
}
