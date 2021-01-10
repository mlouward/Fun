using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class DealCardsGame : MonoBehaviour
{
    public CardStack dealer;
    public CardStack player;

    public void OnGUI()
    {
        if (GUI.Button(new Rect(-10, 0, 120, 20), "Hit me!"))
        {
            player.Push(dealer.Pop());
        }
    }
}