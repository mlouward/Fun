using System;
using System.Diagnostics;

namespace MCTS
{
    internal class Program
    {
        private static void PlayVsAI(int duration)
        {
            Board b = new Board();
            b.PrintBoard();
            int player = Board.P1;
            int res;

            while (true)
            {
                // AI
                b = MCTS.FindNextMove(b, player, duration);
                b.PrintBoard();
                Console.WriteLine($"Nodes explored: {(float)MCTS.ExploredNodes} ({MCTS.ExploredNodes / duration}/sec).");
                res = b.CheckStatus();
                player = 3 - player;
                if (res != -1)
                    break;

                // Human
                Console.WriteLine($"Entrez une position valide entre 1 et {Board.DEFAULT_BOARD_LENGTH}:");
                int joueur = int.Parse(Console.ReadLine()) - 1;
                Position j = new Position(joueur);
                b.PerformMove(player, j);
                b.PrintBoard();
                res = b.CheckStatus();
                player = 3 - player;
                if (res != -1)
                    break;
            }
            Console.WriteLine("Game Over");
            switch (res)
            {
                case 0:
                    Console.WriteLine("Draw!");
                    break;

                case 1:
                    Console.WriteLine("P1 Wins!");
                    break;

                case 2:
                    Console.WriteLine("P2 Wins!");
                    break;

                default:
                    throw new SystemException();
            }
        }

        private static void PlaySelf(int duration)
        {
            Board b = new Board();
            b.PrintBoard();
            int player = Board.P1;
            int res;

            while (true)
            {
                b = MCTS.FindNextMove(b, player, duration);
                player = 3 - player; // Alternate player
                b.PrintBoard();
                Console.WriteLine($"Nodes explored: {(float)MCTS.ExploredNodes} ({MCTS.ExploredNodes / duration}/sec).");
                res = b.CheckStatus();
                if (res != -1)
                    break;
            }
            Console.WriteLine("Game Over");
            switch (res)
            {
                case 0:
                    Console.WriteLine("Draw!");
                    break;

                case 1:
                    Console.WriteLine("P1 Wins!");
                    break;

                case 2:
                    Console.WriteLine("P2 Wins!");
                    break;

                default:
                    throw new SystemException();
            }
        }

        private static void Main(string[] args)
        {
            PlaySelf(10);
            //PlayVsAI(20);
            Console.ReadKey();
        }
    }
}