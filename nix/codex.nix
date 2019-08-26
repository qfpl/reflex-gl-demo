let
  codexPin = builtins.fromJSON (builtins.readFile ./codex.json);

  codex = builtins.fetchGit {
    inherit (codexPin) url rev;
    ref = "master";
  };
in
  codex
