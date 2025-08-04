import {
    Box,
    Drawer,
    List,
    ListItem,
    ListItemButton,
    ListItemText,
    Button,
} from "@mui/material";

interface SidebarProps {
    open: boolean;
    isDesktop: boolean;
    showAccount: boolean;
    onHome: () => void;
    onMyRecipes: () => void;
    onClose: () => void;
    onLogout: () => void;
}

const Sidebar: React.FC<SidebarProps> = ({
    open,
    isDesktop,
    showAccount,
    onHome,
    onMyRecipes,
    onClose,
    onLogout,
}) => (
    <Drawer
        variant={isDesktop ? "permanent" : "temporary"}
        open={open}
        onClose={onClose}
        ModalProps={{ keepMounted: true }}
        sx={{
            width: 260,
            flexShrink: 0,
            [`& .MuiDrawer-paper`]: {
                width: 260,
                boxSizing: "border-box",
                pt: 0,
                display: "flex",
                flexDirection: "column",
                alignItems: "flex-start",
                justifyContent: "flex-start",
                backgroundColor: "var(--background-color)", // Ensure it matches app background
            },
        }}
    >
        <Box
            sx={{
                width: 260,
                pt: 2,
                flex: 1,
                display: "flex",
                flexDirection: "column",
                justifyContent: "flex-start",
            }}
        >
            <List sx={{ width: "100%" }}>
                <ListItem disablePadding>
                    <ListItemButton selected={!showAccount} onClick={onHome}>
                        <ListItemText
                            primary="Home"
                            slotProps={{
                                primary: {
                                    color: "#ff3b5c",
                                    fontWeight: 600,
                                    fontSize: 18,
                                },
                            }}
                        />
                    </ListItemButton>
                </ListItem>
                <ListItem disablePadding>
                    <ListItemButton
                        selected={showAccount}
                        onClick={onMyRecipes}
                    >
                        <ListItemText
                            primary="My Recipes"
                            slotProps={{
                                primary: {
                                    color: "#222",
                                    fontWeight: 600,
                                    fontSize: 18,
                                },
                            }}
                        />
                    </ListItemButton>
                </ListItem>
            </List>
            <Box sx={{ flexGrow: 1 }} />
            <Button
                variant="outlined"
                color="primary"
                onClick={onLogout}
                sx={{
                    width: "90%",
                    mb: 2,
                    alignSelf: "center",
                    borderColor: "#ff3b5c",
                    color: "#ff3b5c",
                    fontWeight: 700,
                }}
            >
                Logout
            </Button>
        </Box>
    </Drawer>
);

export default Sidebar;
