# Access
[![Build Status](https://travis-ci.org/geraud/access.svg?branch=master)] (https://travis-ci.org/geraud/access)

`access` is a command line tool for anyone using Amazon Web Services (AWS) who wishes to list or connect to machines.
It is configured through a simple rc file.

## Usage

`access` has two modes of operation:

0. it can be used to list instances matching one or more criteria.
0. it can be used to connect to a machine based on these criteria.

### List instances

```bash
$ access list

name               account  private_ip   public_dns                               public_ip      instance_type  region
machine-06591b2a   default  10.10.6.123  ec2-10-10-6-123.compute-1.amazonaws.com  54.80.123.123  c3.large       us-east-1
machine-08c82bf5   default  10.10.6.123  ec2-10-10-6-123.compute-1.amazonaws.com  54.80.123.123  c3.large       us-east-1
machine-62dc759e   default  10.10.6.123  ec2-10-10-6-123.compute-1.amazonaws.com  54.80.123.123  c3.large       us-east-1
bla-62dc759e       default  10.10.6.123  ec2-10-10-6-123.compute-1.amazonaws.com  54.80.123.123  c3.large       us-east-1
```

To select specific instances you can type one or more of its attributes.
For instance, if you want to get a list of all machines which name starts with 'bla', type:

```bash
$ access list bla

name           account  private_ip   public_dns                               public_ip      instance_type  region
bla-62dc759e   default  10.10.6.123  ec2-10-10-6-123.compute-1.amazonaws.com  54.80.123.123  c3.large       us-east-1
```

or to list the c3.large instances in your account, type:

```
$ access list c3.large

name               account  private_ip   public_dns                               public_ip      instance_type  region
machine-06591b2a   default  10.10.6.123  ec2-10-10-6-123.compute-1.amazonaws.com  54.80.123.123  c3.large       us-east-1
machine-08c82bf5   default  10.10.6.123  ec2-10-10-6-123.compute-1.amazonaws.com  54.80.123.123  c3.large       us-east-1
machine-62dc759e   default  10.10.6.123  ec2-10-10-6-123.compute-1.amazonaws.com  54.80.123.123  c3.large       us-east-1
bla-62dc759e       default  10.10.6.123  ec2-10-10-6-123.compute-1.amazonaws.com  54.80.123.123  c3.large       us-east-1
```

### Connect to a machine

The main goal of access is to reach any running instances quickly.
Connect to a machine by any attribute (name public_ip ...)

```bash

$ access machine-62dc759e
Connecting 10.10.6.123 in us-east-1 (ec2-10-10-6-123.compute-1.amazonaws.com)

$ access 10.10.6.123
Connecting 10.10.6.123 in us-east-1 (ec2-10-10-6-123.compute-1.amazonaws.com)
```

## Configuration

`access` uses [configurator] (https://github.com/bos/configurator) by
Bryan O'Sullivan for its configuration. Its language is inspired by configgy.
Please refer to the module documentation for additional details; it has really neat features.

There is one gotcha to be aware of: in the command section, entries starting with a single dollar sign "$"
will need to be escaped by "$$".

The configuration is loaded from `$HOME/.accessrc`

#### Important fields

The access section of the configuration requires 4 entries
* `accounts`: names of the account configuration sections
* `fields`: list of fields selected for display (see below).
* `sort_by`: list of the names of the account configuration sections
* `command`: is a command to execute. This command will run in a shell.

The account section of the aws configuration requires 3 entries
* `access_key_id` and `secret_access_key`: your credentials of the account.
* `regions`: regions to consider when scanning an account.

#### Single account configuration

If there is only one account then the configuration would look like this:
```
access {
  accounts = ["default"]
  fields = ["name", "account", "private_ip", "public_dns", "public_ip", "instance_type", "region", "aws:cloudformation:stack-name"]
  sort_by = ["name"]
  command = "ssh -A -p 22 $$PUBLIC_DNS"
}

default {
  access_key_id = "AK..."
  secret_access_key = "..."
  regions = ["us-east-1" "us-west-2"]
}
```

#### Multiple account configuration

If you have both a production and a staging account,  you can just add a new
entry in the configuration file.

```
access {
  accounts = ["production" "staging"]
  fields = ["name", "account", "private_ip", "public_dns", "public_ip", "instance_type", "region", "aws:cloudformation:stack-name"]
  sort_by = ["name"]
  command = "ssh -A -p 22 $$PUBLIC_DNS"
}

production {
  access_key_id = "AK..."
  secret_access_key = "..."
  regions = ["us-east-1"]
}

staging {
  access_key_id = "AK..."
  secret_access_key = "..."
  regions = ["us-west-2"]
}
```

The field section refers to data about the instance. The fields can be picked from the list:
* `account`
* `architecture`
* `availibility_zone`
* `hypervisor_type`
* `image_id`
* `instance_id`
* `instance_type`
* `name`
* `private_dns`
* `private_ip`
* `public_dns`
* `public_ip`
* `region`
* `virtualization_type`
* `vpc_id`
