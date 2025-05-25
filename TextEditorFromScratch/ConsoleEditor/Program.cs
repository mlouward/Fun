using System;

namespace ConsoleEditor;

class Program
{
    static void Main(string[] args)
    {
        Console.TreatControlCAsInput = true;
        var E = new TextEditor();
        try
        {
            while (true)
            {
                E.EditorRefreshScreen();
                E.EditorProcessKeypress();
            }
        }
        catch (Exception ex)
        {
            E.Die(ex);
        }
    }
}
