using UnityEngine;

public class CardModel : MonoBehaviour
{
    private SpriteRenderer spriteRenderer;
    public Sprite[] faces;
    public Sprite back;

    // index = 4 * value + color; except for wild cards (53 - 56)
    // value = index // 4; color = index % 4            (0 - 52)
    public int cardIndex;

    public void ToggleFace(bool showFace)
    {
        if (showFace)
            spriteRenderer.sprite = faces[cardIndex];
        else
            spriteRenderer.sprite = back;
    }

    public void Awake()
    {
        spriteRenderer = GetComponent<SpriteRenderer>();
    }
}