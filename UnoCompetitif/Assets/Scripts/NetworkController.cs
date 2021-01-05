using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using Photon.Pun;
using Photon.Realtime;
using UnityEngine;

namespace Assets.Scripts
{
    public class NetworkController : MonoBehaviourPunCallbacks
    {
        public int PlayersReady { get; set; }

        private void Start()
        {
            PhotonNetwork.ConnectUsingSettings();
        }

        public override void OnConnectedToMaster()
        {
            Debug.Log($"Connected to {PhotonNetwork.CloudRegion}");
            Debug.Log($"{PhotonNetwork.CountOfPlayers} player(s) in the lobby.");

            RoomOptions roomOptions = new RoomOptions();
            roomOptions.IsVisible = false;
            roomOptions.MaxPlayers = 20;
            bool tmp = false;
            try
            {
                tmp = PhotonNetwork.JoinOrCreateRoom("Room", roomOptions, TypedLobby.Default);
                Debug.Log("Joining the room...");
            }
            catch (Exception e)
            {
                Debug.Log("Could not connect to the room...");
                Debug.Log(e);
                Debug.LogError(e);
            }
            if (tmp)
                Debug.Log("Joined the room.");
        }
    }
}