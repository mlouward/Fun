using System;

public delegate void CardRemovedEventHandler(object sender, CardRemovedEventArgs e);

public class CardRemovedEventArgs : EventArgs
{
    public Card CardRemoved { get; private set; }

    public CardRemovedEventArgs(Card card)
    {
        CardRemoved = card;
    }
}