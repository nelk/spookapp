# TODO: Do prod and sandbox versions.
let
  # TODO: Had trouble accessing database from webserver when one was on 192.168.4.0/24 and other ended up on 10.0.0.0/8.
  addressRange = "192.168.4.0/24";
  # addressRange = "10.0.0.0/8";
  # addressRange = "10.138.0.0/20";

  defaultDeployment = { machineName, diskSize, instanceType ? "f1-micro", ipAddress ? null, tags ? [], network ? null }: {
    deployment.targetEnv = "gce";
    deployment.gce = {
      # instance properties
      region = "us-west1-b";

      inherit instanceType;
      inherit machineName;
      inherit ipAddress;
      inherit tags;
      inherit network;
      rootDiskSize = diskSize;

      scheduling.automaticRestart = true;
      scheduling.onHostMaintenance = "MIGRATE";
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
  /* TODO:
  Putting a manually-created static IP resource under NixOps management is done this way: create a resource to temporarily hold the IP address, such as an instance or a forwarding rule; delete the static IP resource, which still leaves the IP address itself under your control thanks to the holding resource; create a new static IP address with resources.gceStaticIPs.$NAME.ipAddress set to the IP address of the holding resource; delete the holding resource after checking that the static IP resource has been correctly created and holds the original IP address. You must practice the migration procedure on a test static IP resource.

If by accident or after ignoring the above advice, you lose control of a valuable IP address, you must act very fast and attempt to create a new static IP resource with with resources.gceStaticIPs.$NAME.ipAddress set to the IP address itself that you want to regain control over. If you are late and the IP address has been given to someone else, it still makes sense to repeately try reserving the address because most likely it is in use as an emphemeral one and thus will become available soon. Needless to say, you want to avoid a situation like this at all costs.

IP addresses are region-specific and thus most likely can't be migrated to another region. It is impossible to migrate an IP address to another project without temporarily losing control over it.
*/

  # resources.gceStaticIPs.spookIp = {
  #   name = "spook-ip";
  #   region = "us-west1";
  #   ipAddress = "35.197.4.149";
  #   publicIPv4 = "35.197.4.149";
  # };

  resources.gceNetworks.spooknet = {
    name = "spooknet";
    inherit addressRange;
    firewall = {
      allow-http = {
        targetTags = ["public-http"];
        allowed.tcp = [80];
      };
      allow-https = {
        targetTags = ["public-https"];
        allowed.tcp = [443];
      };
      allow-internal = {
        allowed.tcp = "0-65535";
        sourceRanges = [addressRange];
      };
    };
  };

  # TODO: Health checks.

  database = {resources, ...}: defaultDeployment {
    machineName = "spook-database-prod";
    diskSize = 20;
    network = resources.gceNetworks.spooknet;
  };

  webserver = {resources, ...}: defaultDeployment {
    machineName = "spook-webserver-prod";
    instanceType = "g1-small";
    diskSize = 10;
    # ipAddress = resources.gceStaticIPs.spookIp;
    ipAddress = "spook-ip";
    tags = ["public-http" "public-https"];
    network = resources.gceNetworks.spooknet;
  };
}
