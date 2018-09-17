# TODO: Do prod and sandbox versions.
let
  defaultDeployment = { diskSize ? 10, ipAddress ? null }: {
    deployment.targetEnv = "gce";
    deployment.gce = {
      # credentials
      project = "spook-216615";
      serviceAccount = "nixops-deployer@spook-216615.iam.gserviceaccount.com";
      accessKey = "./pkey.pem";

      # instance properties
      region = "us-west1-b";
      instanceType = "f1-micro";
      tags = ["prod"];
      scheduling.automaticRestart = true;
      scheduling.onHostMaintenance = "MIGRATE";

      inherit ipAddress;
      rootDiskSize = diskSize;
    };

    # fileSystems."/data" = {
    #   autoFormat = true;
    #   fsType = "ext4";
    #   gce.size = diskSize;
    #   gce.encrypt = true;
    #   gce.disk_name = "data";
    # };
  };
in {
  resources.gceStaticIps = {
  };

  database = defaultDeployment { diskSize = 20; };

  webserver = defaultDeployment {
    diskSize = 10;
    ipAddress = "spook-ip";
  };
}
