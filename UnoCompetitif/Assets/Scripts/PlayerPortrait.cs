using System.Xml.Schema;
using UnityEngine;
using UnityEngine.UI;
using UnityEditor;

public class PlayerPortrait : MonoBehaviour
{
    public Image cbody;
    public Image cface;
    public Image chair;
    public Image ckit;
    public Sprite[] body;
    public Sprite[] face;
    public Sprite[] hair;
    public Sprite[] kit;

    public Sprite avatar;

    // Use this for initialization
    private void Awake()
    {
        RandomizeCharacter();
    }

    public void RandomizeCharacter()
    {
        cbody.sprite = body[Random.Range(0, body.Length)];
        cface.sprite = face[Random.Range(0, face.Length)];
        chair.sprite = hair[Random.Range(0, hair.Length)];
        ckit.sprite = kit[Random.Range(0, kit.Length)];
        avatar = CombineImages();
    }

    public Sprite CombineImages()
    {
        var cbody_copy = duplicateTexture(cbody.sprite.texture);
        var cface_copy = duplicateTexture(cface.sprite.texture);
        var chair_copy = duplicateTexture(chair.sprite.texture);
        var ckit_copy = duplicateTexture(ckit.sprite.texture);
        // Set those two or get them from one the the sprites you want to combine
        int spritesWidth = (int)cbody.sprite.rect.width;
        int spritesHeight = (int)cbody.sprite.rect.height;

        Texture2D combinedTexture = new Texture2D(spritesWidth, spritesHeight);

        for (int x = 0; x < spritesWidth; x++)
        {
            for (int y = 0; y < spritesHeight; y++)
            {
                combinedTexture.SetPixel(x, y, cbody_copy.GetPixel(x, y));
                combinedTexture.SetPixel(x, y, cface_copy.GetPixel(x, y));
                combinedTexture.SetPixel(x, y, chair_copy.GetPixel(x, y));
                combinedTexture.SetPixel(x, y, ckit_copy.GetPixel(x, y));
            }
        }
        combinedTexture.Apply();

        return Sprite.Create(combinedTexture, new Rect(0.0f, 0.0f, combinedTexture.width, combinedTexture.height), new Vector2(0.5f, 0.5f), 100.0f);
    }

    private Texture2D duplicateTexture(Texture2D source)
    {
        RenderTexture renderTex = RenderTexture.GetTemporary(
                    source.width,
                    source.height,
                    0,
                    RenderTextureFormat.Default,
                    RenderTextureReadWrite.Linear);

        Graphics.Blit(source, renderTex);
        RenderTexture previous = RenderTexture.active;
        RenderTexture.active = renderTex;
        Texture2D readableText = new Texture2D(source.width, source.height);
        readableText.ReadPixels(new Rect(0, 0, renderTex.width, renderTex.height), 0, 0);
        readableText.Apply();
        RenderTexture.active = previous;
        RenderTexture.ReleaseTemporary(renderTex);
        return readableText;
    }
}