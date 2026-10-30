# Deployment files

Example files for serving PlaViSCA to several users at once: a few app
instances, one R process each, behind an nginx reverse proxy. The setup
guide (`manual/docs/source/setup_guide.md`, section *Serving several
users*) explains the setup and the alternatives.

| File | Role |
|---|---|
| `systemd/plavisca@.service` | template unit: `plavisca@3838` runs one instance on port 3838 |
| `nginx/plavisca.conf` | nginx server block: sticky sessions, websocket upgrade, gzip |

Both files use placeholder paths (`/path/to/PlaViSCA-app`,
`/path/to/app_data`) and host name (`plavisca.example.org`); edit them
before installing. Both files are ASCII.
