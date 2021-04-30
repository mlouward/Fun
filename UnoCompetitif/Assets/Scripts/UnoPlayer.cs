using System.Collections.Generic;

namespace Assets.Scripts
{
    public class UnoPlayer : Photon.Realtime.Player
    {
        public List<Card> Hand { get; set; }

        public UnoPlayer(int id, string name, List<Card> hand, bool isLocal) : base(name, id, isLocal)
        {
            Hand = hand;
        }
    }
}