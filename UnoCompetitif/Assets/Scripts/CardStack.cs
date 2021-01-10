using System.Collections.Generic;
using System.Linq;
using UnityEngine;

public class CardStack : MonoBehaviour
{
    private List<Card> cards;
    public bool isGameDeck;

    public bool HasCards
    {
        get { return cards != null && cards.Count > 0; }
    }

    public int Count
    {
        get { return cards is null ? 0 : cards.Count; }
    }

    public event CardRemovedEventHandler CardRemoved;

    public IEnumerable<Card> GetCards()
    {
        foreach (Card c in cards)
        {
            yield return c;
        }
    }

    // Draw card
    public Card Pop()
    {
        Card temp = cards[0];
        cards.RemoveAt(0);

        CardRemoved?.Invoke(this, new CardRemovedEventArgs(temp));

        return temp;
    }

    // Add card to the stack
    public void Push(Card card)
    {
        cards.Add(card);
    }

    public void CreateDeck()
    {
        // index = 4 * value + color; except for wild cards (53 - 56)
        // value = index // 4; color = index % 4            (0 - 52)
        cards.Clear();
        // Populate Deck
        // All colored cards  x2
        for (int i = 0; i < 52; i++)
        {
            cards.Add(new Card((CardColors)(i % 4), i / 4, gameObject));
        }
        // Wild and +4 Wild   x4
        for (int i = 0; i < 4; i++)
        {
            cards.Add(new Card(52 % 4, 52 / 4, gameObject));
            cards.Add(new Card((CardColors)(53 % 4), 53 / 4, gameObject));
        }
        // +2 and +1 wild     x1
        cards.Add(new Card((CardColors)(54 % 4), 54 / 4, gameObject));
        cards.Add(new Card((CardColors)(55 % 4), 55 / 4, gameObject));

        // Shuffle
        cards = cards.OrderBy(a => Random.value).ToList();
    }

    private void Awake()
    {
        cards = new List<Card>();
        if (isGameDeck)
        {
            CreateDeck();
        }
    }
}