using UnityEngine;

public class DebugChangeCard : MonoBehaviour
{
    private CardFlipper flipper;
    private CardModel cardModel;
    private int cardIndex = 0;

    public GameObject card;

    private void Awake()
    {
        cardModel = card.GetComponent<CardModel>();
        flipper = card.GetComponent<CardFlipper>();
    }

    private void OnGUI()
    {
        if (GUI.Button(new Rect(10, 10, 100, 28), "Hit me!"))
        {
            if (cardIndex == cardModel.faces.Length)
            {
                cardIndex = -1;
                flipper.FlipCard(cardModel.faces[cardModel.faces.Length - 1], cardModel.back, -1);
            }
            else
            {
                if (cardIndex > 0)
                    flipper.FlipCard(cardModel.faces[cardIndex - 1], cardModel.faces[cardIndex], cardIndex);
                else
                    flipper.FlipCard(cardModel.back, cardModel.faces[cardIndex], cardIndex);
            }
            cardIndex++;
        }
    }
}