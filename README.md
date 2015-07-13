# narcissome-beacon

<a href="https://travis-ci.org/mlin/narcissome-beacon"><img src="https://travis-ci.org/mlin/narcissome-beacon.svg"/></a>

[Beacon Project](http://ga4gh.org/#/beacon)

[beacon.avdl](https://github.com/ga4gh/schemas/blob/master/src/main/resources/avro/beacon.avdl)

Deployment on www15: `sudo /etc/mlin.net_playbooks/execute && sudo ansible-playbook -i localhost, /etc/mlin.net_playbooks/run_role.yml -e "ROLE=narcissome-beacon" -e "TAG=vX.Y.Z"`
