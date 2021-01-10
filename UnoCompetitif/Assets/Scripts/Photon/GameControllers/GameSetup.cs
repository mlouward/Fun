using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class GameSetup : MonoBehaviour
{
    public static GameSetup GS;
    public Transform[] spawnPoint;

    private void OnEnable()
    {
        if (GameSetup.GS is null)
        {
            GameSetup.GS = this;
        }
    }
}