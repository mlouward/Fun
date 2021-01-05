using System.Collections.Generic;

namespace Assets.Scripts
{
    public class Player : Photon.Realtime.Player
    {
        public List<Card> Hand { get; set; }

        public Player(int id, string name, List<Card> hand, bool isLocal) : base(name, id, isLocal)
        {
            Hand = hand;
        }
    }
}