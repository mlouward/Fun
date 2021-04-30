using System.Collections;
using System.Collections.Generic;
using System.IO;
using Photon.Pun;
using UnityEngine;

public class PhotonPlayer : MonoBehaviour
{
    private PhotonView PV;
    public GameObject myAvatar;

    private void Start()
    {
        PV = GetComponent<PhotonView>();
        if (PV.IsMine)
        {
            //myAvatar = PhotonNetwork.Instantiate(Path.Combine("PhotonPrefabs", "avatar.png"),
            //    GameSetup.GS.spawnPoint[0].position, GameSetup.GS.spawnPoint[0].rotation, 0);
        }
    }
}