using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace MCTS
{
    public class Board
    {
        public int[][] boardValues;
        public int totalMoves;

        public static readonly int DEFAULT_BOARD_LENGTH = 7;
        public static readonly int DEFAULT_BOARD_HEIGHT = 6;

        public const int IN_PROGRESS = -1;
        public const int DRAW = 0;
        public const int P1 = 1;
        public const int P2 = 2;

        public Board()
        {
            boardValues = new int[DEFAULT_BOARD_HEIGHT][];
            for (int i = 0; i < DEFAULT_BOARD_HEIGHT; i++)
                boardValues[i] = new int[DEFAULT_BOARD_LENGTH];
        }

        /// <summary>
        /// Creates a Board object with a deep copy of the given array.
        /// </summary>
        /// <param name="board"></param>
        public Board(int[][] board)
        {
            int n = DEFAULT_BOARD_HEIGHT;
            int m = DEFAULT_BOARD_LENGTH;
            int[][] tab = new int[n][];
            for (int i = 0; i < n; i++)
            {
                tab[i] = new int[m];
                for (int j = 0; j < m; j++)
                {
                    tab[i][j] = board[i][j];
                }
            }
            this.boardValues = tab;
        }

        public Board(int[][] boardValues, int totalMoves)
        {
            this.boardValues = boardValues;
            this.totalMoves = totalMoves;
        }

        internal int[][] Result(Position p, int player)
        {
            Debug.Assert(boardValues[0][p.X] == 0);
            int n = DEFAULT_BOARD_HEIGHT;
            int m = DEFAULT_BOARD_LENGTH;
            int[][] newTab = new int[n][];
            for (int i = 0; i < n; i++)
            {
                newTab[i] = new int[m];
                for (int j = 0; j < m; j++)
                {
                    newTab[i][j] = boardValues[i][j];
                }
            }
            for (int j = 0; j < n; j++)
            {
                if (newTab[m - 1 - j][p.X] == 0)
                {
                    newTab[m - 1 - j][p.X] = player;
                    break;
                }
            }
            return newTab;
        }

        public Board(Board board)
        {
            int n = DEFAULT_BOARD_HEIGHT;
            int m = DEFAULT_BOARD_LENGTH;
            this.boardValues = new int[n][];
            for (int i = 0; i < n; i++)
            {
                this.boardValues[i] = new int[m];
                for (int j = 0; j < m; j++)
                {
                    this.boardValues[i][j] = board.boardValues[i][j];
                }
            }
        }

        /// <summary>
        /// Returns the status of the game.
        /// </summary>
        /// <returns> 1 if P1 wins, 2 if P2 wins, 0 for Draw, -1 otherwise. </returns>
        public int CheckStatus()
        {
            //Lignes
            int n = DEFAULT_BOARD_HEIGHT;
            int m = DEFAULT_BOARD_LENGTH;
            for (int j = 0; j < m - 3; j++)
            {
                for (int i = 0; i < n; i++)
                {
                    var t = new List<int> { boardValues[i][j], boardValues[i][j + 1], boardValues[i][j + 2], boardValues[i][j + 3] };
                    if (t.All(s => s == 1) || t.All(s => s == 2))
                        return boardValues[i][j];
                }
            }
            //Colonnes
            for (int i = 0; i < 3; i++)
            {
                for (int l = 0; l < m; l++)
                {
                    var t = new List<int> { boardValues[i][l], boardValues[i + 1][l], boardValues[i + 2][l], boardValues[i + 3][l] };
                    if (t.All(s => s == 1) || t.All(s => s == 2))
                        return boardValues[i][l];
                }
            }
            //Diagonales négatives
            for (int j = 0; j < m - 3; j++)
            {
                for (int i = 0; i < 3; i++)
                {
                    var t = new List<int> { boardValues[i][j], boardValues[i + 1][j + 1], boardValues[i + 2][j + 2], boardValues[i + 3][j + 3] };
                    if (t.All(s => s == 1) || t.All(s => s == 2))
                        return boardValues[i][j];
                }
            }
            //diag positives
            for (int j = 0; j < m - 3; j++)
            {
                for (int i = n - 3; i < n; i++)
                {
                    var t = new List<int> { boardValues[i][j], boardValues[i - 1][j + 1], boardValues[i - 2][j + 2], boardValues[i - 3][j + 3] };
                    if (t.All(s => s == 1) || t.All(s => s == 2))
                        return boardValues[i][j];
                }
            }
            if (GetEmptyPositions().Count > 0)
                return IN_PROGRESS;

            return DRAW;
        }

        public List<Position> GetEmptyPositions()
        {
            int m = DEFAULT_BOARD_LENGTH;
            List<Position> positions = new List<Position>();
            for (int i = 0; i < m; i++)
            {
                if (boardValues[0][i] == 0)
                    positions.Add(new Position(i));
            }
            return positions;
        }

        public void PerformMove(int player, Position p)
        {
            totalMoves++;
            Debug.Assert(boardValues[0][p.X] == 0);
            int n = DEFAULT_BOARD_HEIGHT;
            for (int j = 0; j < n; j++)
            {
                if (boardValues[n - 1 - j][p.X] == 0)
                {
                    boardValues[n - 1 - j][p.X] = player;
                    break;
                }
            }
        }

        public void PrintBoard()
        {
            int n = DEFAULT_BOARD_HEIGHT;
            int m = DEFAULT_BOARD_LENGTH;
            string col = "";
            for (int i = 0; i < m; i++)
            {
                col += $"{i + 1} ";
            }
            Console.WriteLine(col);
            for (int i = 0; i < n; i++)
            {
                for (int j = 0; j < m; j++)
                {
                    switch (boardValues[i][j])
                    {
                        case 1:
                            Console.Write("O ");
                            break;

                        case 2:
                            Console.Write("X ");
                            break;

                        default:
                            Console.Write(". ");
                            break;
                    }
                }
                Console.WriteLine();
            }
            Console.WriteLine();
        }

        public void PrintStatus()
        {
            switch (CheckStatus())
            {
                case P1:
                    Console.WriteLine("Player 1 wins");
                    break;

                case P2:
                    Console.WriteLine("Player 2 wins");
                    break;

                case DRAW:
                    Console.WriteLine("Game Draw");
                    break;

                case IN_PROGRESS:
                    Console.WriteLine("Game In Progress");
                    break;

                default:
                    Console.WriteLine($"Error when reading Status: {CheckStatus()}");
                    break;
            }
        }
    }
}