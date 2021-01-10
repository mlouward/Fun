using System;
using UnityEngine;
using UnityEngine.UI;

public enum CardColors
{
    // 0: Red, 1: Yellow, 2: Green, 3: Blue, 4: Wild

    Red,
    Yellow,
    Green,
    Blue,
    Wild,
}

public class Card : MonoBehaviour
{
    /*
    * 0-9 are regular
    * 10 is skip
    * 11 is reverse
    * 12 is draw 2
    * 13 is color change
    * 14 is +4 color change
    * 15 is +2 color change
    * 16 is +1 color change
    */

    public int Value { get; set; }
    public CardColors Color { get; set; }
    public GameObject CardObj { get; set; }

    public Card(CardColors color, int number, GameObject gameObject)
    {
        Value = number;
        Color = color;
        CardObj = gameObject;
    }

    public GameObject LoadCard(int x, int y, Transform parent)
    {
        //when ran, it tells where to load the card on the screen
        GameObject temp = LoadCard(parent);
        temp.transform.localPosition = new Vector2(x, y + 540);
        return temp;
    }

    public GameObject LoadCard(Transform parent)
    {
        //does all the setup for loading. Used if card doesn't need a specific position
        GameObject temp = Instantiate(CardObj);
        temp.name = Color.ToString() + Value;
        if (Value < 10)
        {
            foreach (Transform childs in temp.transform)
            {
                if (childs.name.Equals("Cover"))
                    break;
            }
        }

        temp.GetComponent<RawImage>().texture = Resources.Load($"CardAssets/Card{Value}_{(int)Color}") as Texture2D;
        temp.transform.SetParent(parent);
        temp.transform.localScale = new Vector3(1, 1, 1);
        return temp;
    }

    public bool Equals(Card other)
    {
        //overides the original Equals so that color or number must be equal
        return other.Value == Value || other.Color.Equals(Color);
    }

    public bool CanCutOnCard(Card other)
    {
        // Used to detect if card can be placed on 'other' at any time (cut)
        return other.Value == Value && other.Color.Equals(Color);
    }
}