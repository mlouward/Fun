namespace MCTS
{
    public class Tree
    {
        public Node Root { get; set; }

        public Tree()
        {
            Root = new Node();
        }

        public Tree(Node root)
        {
            Root = root;
        }
    }
}