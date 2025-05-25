using NUnit.Framework;
using ConsoleEditor; // Assuming your TextEditor class is in this namespace

namespace TextEditorTests
{
    [TestFixture]
    public class EditorMovementTests
    {
        [Test]
        public void MoveCursorDown_MovesDownOneLine()
        {
            var lines = new List<string> { "First line", "Second line", "Third line" };
            var editor = new TextEditor(lines);

            editor.MoveCursorDown();

            Assert.That(editor.CursorY == 1);
            Assert.That(editor.CursorX == 0);
        }

        [Test]
        public void MoveCursorDown_AtLastLine_DoesNotMove()
        {
            var lines = new List<string> { "A", "B" };
            var editor = new TextEditor(lines);
            editor.MoveCursorDown(); // Line 1
            editor.MoveCursorDown(); // Should stay at last line

            Assert.That(1 == editor.CursorY);
        }

        [Test]
        public void MoveCursorUp_MovesUpOneLine()
        {
            var lines = new List<string> { "A", "B", "C" };
            var editor = new TextEditor(lines);
            editor.MoveCursorDown(); // Line 1
            editor.MoveCursorDown(); // Line 2

            editor.MoveCursorUp();

            Assert.That(1 == editor.CursorY);
            Assert.That(0 == editor.CursorX);
        }

        [Test]
        public void MoveCursorUp_AtFirstLine_DoesNotMove()
        {
            var lines = new List<string> { "A", "B" };
            var editor = new TextEditor(lines);

            editor.MoveCursorUp();

            Assert.That(0 == editor.CursorY);
        }

        [Test]
        public void MoveCursorDown_ColumnClampedToLineLength()
        {
            var lines = new List<string> { "12345", "12" };
            var editor = new TextEditor(lines);
            editor.CursorX = 4; // Set to end of first line

            editor.MoveCursorDown();

            Assert.That(1 == editor.CursorY);
            Assert.That(2 == editor.CursorX); // Clamped to length of "12"
        }

        [Test]
        public void MoveCursorUp_ColumnClampedToLineLength()
        {
            var lines = new List<string> { "1", "12345" };
            var editor = new TextEditor(lines);
            editor.MoveCursorDown();
            editor.CursorX = 4; // Set to end of second line

            editor.MoveCursorUp();

            Assert.That(0 == editor.CursorY);
            Assert.That(1 == editor.CursorX); // Clamped to length of "1"
        }
    }
}
