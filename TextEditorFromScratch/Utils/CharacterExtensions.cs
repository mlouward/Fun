using System;

namespace Utils;

public static class ConsoleKeyInfoExtensions
{
    /// <summary>
    /// Returns <see langword="true"/> if the key pressed is a control key and the key pressed is <paramref name="c"/>.
    /// </summary>
    public static bool CtrlKey(this ConsoleKeyInfo input, char c)
    {
        return input.Modifiers.HasFlag(ConsoleModifiers.Control)
        && (char)input.Key == char.ToUpperInvariant(c);
    }
}
