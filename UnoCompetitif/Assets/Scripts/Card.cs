using System;
using UnityEngine;

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

    private CardColors Color { get; }
    private int Value { get; }
    private GameObject CardObj;

    public Card(CardColors color, int number, Vector2 position, Quaternion rotation)
    {
        string assetName = $"Card_{number}_{(int)color}";  // Example:  "Card_9_1" would be the Yellow 9.
        GameObject asset = GameObject.Find(assetName);
        if (asset is null)
            Debug.LogError("Asset '" + assetName + "' could not be found.");
        else
        {
            CardObj = Instantiate(asset, position, rotation);
            Color = color;
            Value = number;
        }
    }
}