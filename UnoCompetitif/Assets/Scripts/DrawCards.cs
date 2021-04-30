using System.Collections.Generic;
using UnityEngine;

public class DrawCards : MonoBehaviour
{
    public GameObject CardUp;
    public GameObject CardDown;

    // List of cards
    public CardStack stack;

    // Players hands

    public GameObject HandArea;
    public GameObject HandArea1;
    public GameObject HandArea2;
    public GameObject HandArea3;
    public GameObject HandArea4;
    public GameObject HandArea5;
    public GameObject HandArea6;
    public GameObject HandArea7;

    public void Start()
    {
    }

    public void OnClick()
    {
        List<GameObject> hands = new List<GameObject>()
        {
            HandArea1,
            HandArea2,
            HandArea3,
            HandArea4,
            HandArea5,
            HandArea6,
            HandArea7,
        };

        for (int j = 0; j < 7; j++)
        {
            GameObject playerCard = Instantiate(stack.gameObject, new Vector3(0, 0, 0), Quaternion.identity);
            playerCard.transform.SetParent(HandArea.transform, false);
            foreach (var hand in hands)
            {
                playerCard = Instantiate(stack.gameObject, new Vector3(0, 0, 0), Quaternion.identity);
                playerCard.transform.SetParent(hand.transform, false);
            }
        }
    }
}