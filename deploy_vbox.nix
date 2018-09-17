let
  deployment = {
    deployment.targetEnv = "virtualbox";
    deployment.virtualbox.headless = 1;
  };
in {
  database = deployment;
  webserver = deployment;
}
