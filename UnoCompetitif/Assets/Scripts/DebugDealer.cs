using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class DebugDealer : MonoBehaviour
{
    public CardStack dealer;
    public CardStack player;

    public void OnGUI()
    {
        if (GUI.Button(new Rect(-10, 10, 256, 28), "Hit me!"))
        {
            player.Push(dealer.Pop());
        }
    }
}