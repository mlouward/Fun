using System;

public static class RotationalCipher
{
	public static char Cipher(char ch, int key)
	{
		if (!char.IsLetter(ch))
			return ch;

		char d = char.IsUpper(ch) ? 'A' : 'a';
		return (char)(((ch + key - d) % 26) + d);
	}

	public static string Rotate(string text, int shiftKey)
	{
		if (shiftKey % 26 == 0)
			return text;
		string res = "";
		foreach (var item in text)
			res += Cipher(item, shiftKey);

		return res;
	}
}