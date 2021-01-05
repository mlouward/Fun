using System.Collections.Generic;
using System.Linq;
using Photon.Realtime;
using UnityEngine;

public class Deck : MonoBehaviour
{
    private List<Card> Draw = new List<Card>();
    private List<Card> Discard = new List<Card>();

    public Deck()
    {
        for (int number = 0; number < 10; number++)
        {
            for (int color = 0; color < 4; color++)
            {
                string assetName = $"Card_{number}_{color}";  // Example:  "Card_9_1" would be the Yellow 9.
                GameObject card = GameObject.Find(assetName);
                Draw.Add(new Card((CardColors)color, number, new Vector2(0, 0), new Quaternion(0, 0, 0, 0)));
            }
        }
    }

    /// <summary>
    /// Shuffles the draw pile (start of game)
    /// </summary>
    public void Shuffle()
    {
        Draw = Draw.OrderBy(a => Random.value).ToList();
    }

    /// <summary>
    /// Used when draw pile is empty to shuffle discard pile.
    /// </summary>
    public void MakeDrawFromDiscard()
    {
        Draw = Discard.OrderBy(a => Random.value).ToList();
        Discard = new List<Card>();
    }

    public Card TakeCard(Player p)
    {
        if (Draw.Count == 0)
            MakeDrawFromDiscard(); // the deck is depleted: reshuffle and continue

        // take the first card off the deck and add it to the discard pile
        Card card = Draw[0];
        Draw.RemoveAt(0);

        Discard.Add(card);

        return card;
    }
}