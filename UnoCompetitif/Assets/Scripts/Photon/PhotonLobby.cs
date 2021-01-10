using System.IO;
using System.Net.NetworkInformation;
using Photon.Pun;
using Photon.Pun.UtilityScripts;
using Photon.Realtime;
using UnityEngine;

namespace Assets.Scripts
{
    public class PhotonLobby : MonoBehaviourPunCallbacks
    {
        public static PhotonLobby lobby;
        public GameObject startButton;
        public GameObject cancelButton;
        public PlayerPortrait portrait;

        private void Awake()
        {
            lobby = this;
        }

        public int PlayersReady { get; set; }

        private void Start()
        {
            PhotonNetwork.ConnectUsingSettings();
        }

        private void CreateRoom()
        {
            int randomRoomName = Random.Range(0, 10000);
            RoomOptions roomOptions = new RoomOptions
            {
                IsVisible = true,
                IsOpen = true,
                MaxPlayers = 8
            };

            PhotonNetwork.CreateRoom($"Room {randomRoomName}", roomOptions);
        }

        public void OnStartButtonClicked()
        {
            startButton.SetActive(false);
            cancelButton.SetActive(true);
            // TODO: Save Avatar to a png file
            //Sprite portrait = GetComponent<PlayerPortrait>().avatar;
            //Texture2D portraitTexture = portrait.texture;
            //byte[] portraitBytes = portraitTexture.EncodeToPNG();
            //File.WriteAllBytes($"../../Resources/PhotonPrefabs/avatar.png", portraitBytes);
            PhotonNetwork.JoinRandomRoom();
        }

        public override void OnConnectedToMaster()
        {
            Debug.Log($"Connected to {PhotonNetwork.CloudRegion}");
            Debug.Log($"{PhotonNetwork.CountOfPlayers} player(s) in the lobby.");
            PhotonNetwork.AutomaticallySyncScene = true;
            startButton.SetActive(true);
        }

        public override void OnJoinRandomFailed(short returnCode, string message)
        {
            Debug.Log($"Could not join room: {returnCode}: {message}");
            CreateRoom();
        }

        public override void OnCreateRoomFailed(short returnCode, string message)
        {
            Debug.Log($"Could not create room: {returnCode}: {message}");
            CreateRoom();
        }

        public override void OnJoinedRoom()
        {
            Debug.Log("Successfully joined a room!");
            // TODO: Join Main Game scene
        }

        public override void OnLeftRoom()
        {
            Debug.Log("Left the room!");
        }

        public void OnCancelButtonClicked()
        {
            cancelButton.SetActive(false);
            startButton.SetActive(true);
            PhotonNetwork.LeaveRoom();
        }
    }
}