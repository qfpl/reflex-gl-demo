let
  reflex-basic-hostPin = builtins.fromJSON
    (builtins.readFile ./reflex-basic-host.json);

  reflex-basic-host = builtins.fetchGit {
    inherit (reflex-basic-hostPin) url rev;
    ref = "master";
  };
in
  reflex-basic-host
