using System;
using System.Collections.Generic;
using Utils;

namespace ConsoleEditor;

public class TextEditor
{
    public int ScreenRows { get; set; }
    public int ScreenCols { get; set; }
    public int CursorX { get; set; }
    public int CursorY { get; set; }
    /// <summary>
    /// Stores a list of the lines of text to render
    /// </summary>
    List<string> Ab { get; init; }

    public TextEditor()
    {
        (ScreenRows, ScreenCols) = GetWindowSize();
        Ab = [];
        CursorX = Console.CursorLeft;
        CursorY = Console.CursorTop;
    }

    /// <summary>
    /// Constructor that initializes the text editor with a list of initial lines
    /// </summary>
    /// <param name="initialLines">The lines that need to be written (without the tildes)</param>
    public TextEditor(List<string> initialLines)
    {
        (ScreenRows, ScreenCols) = GetWindowSize();
        Ab = initialLines;
        CursorX = Console.CursorLeft;
        CursorY = Console.CursorTop;
    }

    #region terminal

    /// <summary>
    /// Exit the terminal, clear the screen, reposition the cursor
    /// </summary>
    public void Die(Exception e)
    {
        Console.ResetColor();
        Console.CursorVisible = true;
        Console.Clear();
        ResetCursor();
        Console.WriteLine(e);
        Environment.Exit(1);
    }

    public static void ResetCursor()
    {
        SetCursorPosition(0, 0);
    }


    public static void SetCursorPosition(int x, int y)
    {
        Console.CursorLeft = x;
        Console.CursorTop = y;
    }

    public static (int Width, int Height) GetWindowSize()
    {
        return (Console.WindowWidth, Console.WindowHeight);
    }

    public static (int X, int Y) GetCursorPosition => (Console.CursorLeft, Console.CursorTop);

    #endregion

    #region input

    public void EditorProcessKeypress()
    {
        ConsoleKeyInfo keyInfo = Console.ReadKey(true);
        switch (keyInfo.Key)
        {
            case ConsoleKey.UpArrow:
                MoveCursorUp();
                break;
            case ConsoleKey.DownArrow:
                MoveCursorDown();
                break;
            case ConsoleKey.LeftArrow:
                MoveCursorLeft();
                break;
            case ConsoleKey.RightArrow:
                MoveCursorRight();
                break;
            case ConsoleKey.Enter:
                InsertNewLine();
                break;
            case ConsoleKey.Backspace:
                HandleBackspace();
                break;
            default:
                if (keyInfo.CtrlKey('q'))
                {
                    Console.Clear();
                    ResetCursor();
                    Environment.Exit(0);
                }
                else
                {
                    // For now only considers alphanumeric: insert them into the buffer
                    if (char.IsLetterOrDigit(keyInfo.KeyChar) || char.IsWhiteSpace(keyInfo.KeyChar))
                    {
                        // Ensure the current line exists
                        if (CursorY >= Ab.Count)
                        {
                            Ab.Add(string.Empty);
                        }

                        // Insert character at current cursor position
                        var currentLine = Ab[CursorY];
                        if (CursorX < currentLine.Length)
                        {
                            Ab[CursorY] = currentLine.Insert(CursorX, keyInfo.KeyChar.ToString());
                        }
                        else
                        {
                            Ab[CursorY] += keyInfo.KeyChar; // Append to the end of the line
                        }

                        CursorX++;
                    }
                }
                break;
        }
    }

    public void HandleBackspace()
    {
        Ab.RemoveAt(Ab.Count - 1);
    }

    public void InsertNewLine()
    {
        Ab.Add(Environment.NewLine);
    }

    public void MoveCursorRight()
    {
        if (CursorX < Ab[CursorY].Length)
        {
            CursorX++;
        }
        else
        {
            // Prevent moving right of the last character in the line
            CursorX = Ab[CursorY].Length;
        }

        // Handle Y if going to next line
        if (CursorX > Ab[CursorY].Length)
        {
            if (CursorY < ScreenRows - 1 && CursorY < Ab.Count - 1)
            {
                CursorY++;
                CursorX = 0; // Move to the start of the next line
            }
            else
            {
                CursorX = Ab[CursorY].Length; // Stay at the end of the current line
            }
        }
    }

    public void MoveCursorLeft()
    {
        if (CursorX > 0)
        {
            CursorX--;
        }
        else
        {
            // Prevent moving left of the first character
            CursorX = 1;
        }

        // Handle Y if going back up one line
        if (CursorX < 1)
        {
            if (CursorY > 0)
            {
                CursorY--;
                CursorX = Ab[CursorY].Length; // Move to the end of the previous line
            }
            else
            {
                CursorX = 1; // Stay at the start of the first line
            }
        }
    }

    public void MoveCursorUp()
    {
        if (CursorY > 0)
        {
            CursorY--;
        }
        else
        {
            // Prevent moving above the first line
            CursorY = 0;
        }

        // Handle X if line above is shorter than current cursor X
        if (Ab[CursorY].Length < CursorX)
        {
            CursorX = Ab[CursorY].Length;
        }
    }

    public void MoveCursorDown()
    {
        if (CursorY < ScreenRows - 1 && CursorY < Ab.Count - 1)
        {
            CursorY++;
        }
        else
        {
            // Prevent moving below the last line
            CursorY = Math.Min(ScreenRows - 1, Ab.Count - 1);
        }

        // Handle X if line below is shorter than current cursor X
        if (Ab[CursorY].Length < CursorX)
        {
            CursorX = Ab[CursorY].Length;
        }
    }

    #endregion

    #region output

    public void EditorRefreshScreen()
    {
        Console.Clear();
        // Draw tildes at start of each line
        BufferAddRows();

        ResetCursor();

        // Write the buffer
        foreach (var line in Ab)
        {
            Console.Write(line.ToString());
        }

        // Set cursor to its position
        SetCursorPosition(CursorX, CursorY);
    }


    public void BufferAddRows()
    {
        for (int y = 0; y < ScreenCols; y++)
        {
            Ab.Insert(0, "~");

            if (y < ScreenCols - 1)
            {
                Ab.Insert(0, Environment.NewLine);
            }
        }
    }

    #endregion
}
