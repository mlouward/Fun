using System;
using System.Collections.Generic;

namespace MCTS
{
    public class State
    {
        public Board Board { get; set; }
        public int PlayerNo { get; set; }
        public int VisitCount { get; set; }
        public double WinScore { get; set; }
        public int Opponent { get; set; }

        public State()
        {
            Board = new Board();
        }

        public State(Board board)
        {
            Board = board;
        }

        public State(State state)
        {
            Board = new Board(state.Board);
            PlayerNo = state.PlayerNo;
            VisitCount = state.VisitCount;
            WinScore = state.WinScore;
            Opponent = state.PlayerNo;
        }

        public int GetOpponent()
        {
            return 3 - PlayerNo;
        }

        public List<State> GetAllPossibleStates()
        {
            List<State> states = new List<State>();
            this.Board.GetEmptyPositions().ForEach(p =>
            {
                State newState = new State(this) { PlayerNo = GetOpponent() };
                newState.Board.PerformMove(newState.PlayerNo, p);
                states.Add(newState);
            });
            return states;
        }

        public void TogglePlayer()
        {
            PlayerNo = 3 - PlayerNo;
        }

        public void RandomPlay()
        {
            List<Position> availablePositions = Board.GetEmptyPositions();
            int totalPossibilities = availablePositions.Count;
            Random rd = new Random();
            Board.PerformMove(PlayerNo, availablePositions[rd.Next(totalPossibilities)]);
        }

        public void AddScore(int score)
        {
            if (WinScore != int.MinValue)
                WinScore += score;
        }
    }
}