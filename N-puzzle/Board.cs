using System;
using System.Collections.Generic;
using System.Linq;

namespace N_puzzle
{
    internal class Board
    {
        public int Size { get; set; }
        public string Bitmask { get; set; }
        public int Fitness { get; set; }

        // Board is represented as a 0-indexed array of length Size².
        public int this[int i]
        {
            get
            {
                return Convert.ToInt32(Bitmask.Substring(5 * i, 5), 2);
            }
            set
            {
                string nw = "";
                bool done = false;
                // Copy bitmask into nw and change the 5 bits corresponding to i.
                for (int j = 0; j < Bitmask.Length; j += 5)
                {
                    if (!done && j / 5 != i)
                    {
                        nw += Bitmask.Substring(j, 5);
                        done = true;
                    }
                    else
                        nw += Convert.ToString(i, 2);
                }
                Bitmask = nw;
            }
        }

        public Board()
        {
        }

        public Board(string bitmask)
        {
            Bitmask = bitmask;
            Size = (int)Math.Sqrt(bitmask.Length / 5);
            Fitness = GetFitness();
        }

        public override string ToString()
        {
            string s = "";
            for (int i = 0; i < Bitmask.Length; i += 5)
            {
                int tmp = Convert.ToInt32(Bitmask.Substring(i, 5), 2);
                s += tmp == Size * Size ? "|  " : tmp.ToString().Length == 2 ? "|" + tmp : "| " + tmp;

                if ((i + 5) % Size == 0)
                    s += "|\n" + new string('—', 3 * Size) + "\n";
            }
            return s;
        }

        public int GetFitness()
        {
            int res = 0;
            for (int i = 0; i < Size; i++)
            {
                for (int j = 0; j < Size; j++)
                {
                    int tmp = this[i + Size * j] - 1;
                    res += Math.Abs(i - tmp % Size) + Math.Abs(j - tmp / Size);
                }
            }
            return res;
        }

        public Board Move(int originPos, int destPos)
        {
            int o = this[originPos];
            int d = this[destPos];
            string newMask = "";
            for (int i = 0; i < Bitmask.Length; i += 5)
            {
                if (i / 5 == originPos)
                    newMask += Convert.ToString(d, 2).PadLeft(5, '0');
                else if (i / 5 == destPos)
                    newMask += Convert.ToString(o, 2).PadLeft(5, '0');
                else
                    newMask += Bitmask.Substring(i, 5);
            }
            Board copy = new Board(newMask) { Size = Size };

            return copy;
        }

        public Dictionary<Board, int> SelectChildren(int count=2)
        {
            return GetChildren().ToDictionary(k => k, v => v.Fitness).OrderBy(c => c.Value).Take(count)
                .ToDictionary(k => k.Key, v => v.Value);
        }
        public List<Board> GetChildren()
        {
            List<Board> children = new List<Board>();
            for (int i = 0; i < Size; i++)
            {
                for (int j = 0; j < Size; j++)
                {
                    int tmp = i * Size + j;
                    if (this[tmp] == Size * Size)
                    {
                        if (tmp - 1 >= 0 && tmp / Size == (tmp - 1) / Size) // Must be on same line
                            children.Add(Move(tmp, tmp - 1)); // Square on left
                        if (tmp + 1 < Size * Size && tmp / Size == (tmp + 1) / Size) // Must be on same line
                            children.Add(Move(tmp, tmp + 1)); // Square on right
                        if (tmp - Size >= 0)
                            children.Add(Move(tmp, tmp - Size)); // Square over
                        if (tmp + Size < Size * Size)
                            children.Add(Move(tmp, tmp + Size)); // Square under
                    }
                }
            }
            return children;
        }
    }
}