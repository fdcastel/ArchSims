import { Container } from "@chakra-ui/react";
import { ColorModeButton } from "./components/ui/color-mode";

import NeanderEmulator from "./components/Neander/NeanderEmulator";

const App = () => {
  return (
    <Container>
      <ColorModeButton />
      <NeanderEmulator />
    </Container>
  );
};

export default App;
