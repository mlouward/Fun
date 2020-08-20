using System;

namespace N_puzzle
{
    internal class Board
    {
        public int Size { get; set; }
        public string Bitmask { get; set; }

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

        public Board(string bitmask)
        {
            Bitmask = bitmask;
            Size = (int)Math.Sqrt(bitmask.Length / 5);
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

        public int Fitness()
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
    }
}