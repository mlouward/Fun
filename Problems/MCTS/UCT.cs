namespace MCTS
{
    public class UCT
    {
        /// <summary>
        /// Computes UCT value.
        /// </summary>
        /// <param name="totalVisit"></param>
        /// <param name="nodeWinScore"></param>
        /// <param name="nodeVisit"></param>
        /// <returns></returns>
        public static double UctValue(int totalVisit, double nodeWinScore, int nodeVisit)
        {
            // In order not to revisit already expanded children, unvisited nodes are the most valuable
            if (nodeVisit == 0) return int.MaxValue;
            return (nodeWinScore / nodeVisit) + Math.Sqrt(Math.Log(totalVisit) / nodeVisit);
        }

        /// <summary>
        /// Uses UCT Exploration/Exploitation formula to compute the score.
        /// </summary>
        /// <param name="node"> Current node </param>
        /// <returns> Node with highest score amongst children of current node</returns>
        public static Node FindBestNodeWithUCT(Node node)
        {
            int parentVisit = node.State.VisitCount;
            return node.ChildrenList.Aggregate((c1, c2) =>
                UctValue(parentVisit, c1.State.WinScore, c1.State.VisitCount) >
                UctValue(parentVisit, c2.State.WinScore, c2.State.VisitCount) ? c1 : c2);
        }
    }
}