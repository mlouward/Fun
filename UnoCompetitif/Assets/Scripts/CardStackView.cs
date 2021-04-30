using System.Collections.Generic;
using System.Linq;
using UnityEngine;

[RequireComponent(typeof(CardStack))]
public class CardStackView : MonoBehaviour
{
    private CardStack deck;
    private Dictionary<Card, List<GameObject>> fetchedCards;
    private int lastCount;

    public Vector3 start;
    public float cardOffset;
    public bool faceUp = false;
    public GameObject cardPrefab;

    private void Start()
    {
        fetchedCards = new Dictionary<Card, List<GameObject>>();
        deck = GetComponent<CardStack>();
        ShowCards();
        lastCount = deck.Count;

        deck.CardRemoved += Deck_CardRemoved;
    }

    private void Update()
    {
        if (lastCount != deck.Count)
        {
            lastCount = deck.Count;
            ShowCards();
        }
    }

    private void ShowCards()
    {
        int cardCount = 0;
        if (deck.HasCards)
        {
            int index = 0;
            foreach (Card c in deck.GetCards())
            {
                if (c.Color != CardColors.Wild)
                    index = 4 * c.Value + (int)c.Color;
                else
                {
                    // Wild cards are offset by 40
                    index = c.Value + 40;
                }
            }
            float co = cardOffset * cardCount;
            Vector3 temp = start + new Vector3(co, 0f);

            AddCard(temp, index, cardCount);

            cardCount++;
        }
    }

    private void Deck_CardRemoved(object sender, CardRemovedEventArgs e)
    {
        if (fetchedCards.ContainsKey(e.CardRemoved))
        {
            Destroy(fetchedCards[e.CardRemoved].First());
            fetchedCards.Remove(e.CardRemoved);
        }
    }

    private void AddCard(Vector3 position, int cardIndex, int positionalIndex)
    {
        //if (fetchedCards.ContainsKey(cardIndex))
        //return;

        GameObject cardCopy = Instantiate(cardPrefab);
        cardCopy.transform.position = position;

        // toggle to facing camera
        CardModel cardModel = cardCopy.GetComponent<CardModel>();
        cardModel.cardIndex = cardIndex;
        cardModel.ToggleFace(faceUp);

        // Render cards one on top of the other
        SpriteRenderer spriteRenderer = cardCopy.GetComponent<SpriteRenderer>();
        spriteRenderer.sortingOrder = positionalIndex;

        //if (fetchedCards.TryGetValue(cardIndex, out List<GameObject> value))
        //    value.Add(cardCopy);
        //else
        //    fetchedCards.Add(cardIndex, new List<GameObject>() { cardCopy });

        //fetchedCards.Add(cardIndex, cardCopy);
    }
}