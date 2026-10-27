# Actions deployment identity

Website deployment uses GitHub OIDC and Workload Identity Federation through
a dedicated deployment account. It does not use a stored service-account key.
Terry owns the initial GCP IAM and repository-variable setup; the runtime
account remains separate. The deployment uses these resources:

| Resource | Value |
| --- | --- |
| Project / project number | `lizardanalyzer` / `54408718577` |
| GitHub repository / numeric ID | `terryyin/lizard` / `4738329` |
| GitHub owner numeric ID | `788834` |
| Workload Identity pool / provider | `github-actions` / `website-release`, location `global` |
| Deployment account | `website-deployer@lizardanalyzer.iam.gserviceaccount.com` |
| Runtime account | `lizardanalyzer@appspot.gserviceaccount.com` |
| Repository variable `WEBSITE_WIF_PROVIDER` | `projects/54408718577/locations/global/workloadIdentityPools/github-actions/providers/website-release` |
| Repository variable `WEBSITE_DEPLOY_SERVICE_ACCOUNT` | `website-deployer@lizardanalyzer.iam.gserviceaccount.com` |

Enable `iam.googleapis.com`, `iamcredentials.googleapis.com` and
`sts.googleapis.com` for federation, in addition to the existing App Engine,
Cloud Build, Artifact Registry and Storage APIs. The provider's issuer is
`https://token.actions.githubusercontent.com`. Its attribute mapping is:

```text
google.subject=assertion.sub
attribute.repository_id=assertion.repository_id
attribute.repository_owner_id=assertion.repository_owner_id
attribute.ref=assertion.ref
attribute.workflow_ref=assertion.workflow_ref
```

Use numeric repository/owner claims to prevent a renamed or recreated
repository from inheriting trust. The permanent provider condition is:

```text
assertion.repository_id == '4738329' &&
assertion.repository_owner_id == '788834' &&
assertion.ref.startsWith('refs/tags/web-') &&
assertion.workflow_ref == 'terryyin/lizard/.github/workflows/website-release.yml@' + assertion.ref
```

The tag validator must run before requesting credentials: the provider's
prefix restriction is an identity boundary, not the version-format validator.
Grant `roles/iam.workloadIdentityUser` on the deployment account to this member:

```text
principalSet://iam.googleapis.com/projects/54408718577/locations/global/workloadIdentityPools/github-actions/attribute.repository_id/4738329
```

The deployment account has these grants; unrelated bindings are
preserved:

| Scope | Role | Purpose |
| --- | --- | --- |
| Project `lizardanalyzer` | `roles/appengine.deployer` | Read App Engine state and deploy versions |
| Project `lizardanalyzer` | `roles/cloudbuild.builds.editor` | Submit and observe the App Engine source build |
| Project `lizardanalyzer` | `projects/lizardanalyzer/roles/websiteTrafficRouter` | Custom role containing only `appengine.services.update` for verified promotion/recovery |
| Bucket `gs://staging.lizardanalyzer.appspot.com` | `roles/storage.objectAdmin` | Upload source objects in the deployment staging bucket |
| Runtime account only | `roles/iam.serviceAccountUser` | Deploy using the existing runtime identity |

The runtime account retains its existing bucket-level
`roles/storage.objectViewer` grant. The deployment account has no Owner,
Editor, service-account-key or IAM-administration role. Preview deployments
use `--no-promote --no-stop-previous-version`; use a unique version ID because
App Engine Deployer can overwrite an existing serving version.

## Traffic routing permission

Automatic promotion additionally requires `appengine.services.update`; the
existing Deployer grant already supplies service and operation reads. Use a
project custom role containing that single missing permission rather than
App Engine Service Admin (which also permits service deletion):

```sh
gcloud iam roles create websiteTrafficRouter --project=lizardanalyzer --title='Website traffic router' --permissions=appengine.services.update --stage=GA
gcloud projects add-iam-policy-binding lizardanalyzer --member=serviceAccount:website-deployer@lizardanalyzer.iam.gserviceaccount.com --role=projects/lizardanalyzer/roles/websiteTrafficRouter
```

Inspect existing role/policy state before adding or restoring a grant; keep
unrelated bindings. The release CLI addresses only the `default` service and
requires successful preview verification before calling traffic mutation.
See the [service patch permission](https://docs.cloud.google.com/appengine/docs/admin-api/reference/rest/v1/apps.services/patch)
and [traffic command](https://docs.cloud.google.com/sdk/gcloud/reference/app/services/set-traffic).

## Authentication and permission recovery

The provider accepts only the permanent condition above. Local gcloud
credentials do not prove the Actions route works. Allow up to five minutes
for new federation and IAM settings to propagate; inspect an authentication
failure before attempting another run.

For account/permission recovery, inspect the named provider and IAM policies
before changing them. Restore only a missing grant at the scope listed above;
do not grant Editor or reuse the runtime identity as the deployer. GitHub
Actions needs `contents: read` and job-scoped `id-token: write`. Check out the
source before `google-github-actions/auth@v3`, then use
`google-github-actions/setup-gcloud@v3`. Generated `gha-creds-*.json` files are
ignored by Git and excluded by the deployment upload allowlist; never archive
them. The deployment records `LIZARD_WEBSITE_SOURCE_SHA` in the App Engine
version's environment metadata so the tested source can be inspected.

References: [Google's federation setup](https://github.com/google-github-actions/auth#workload-identity-federation-through-a-service-account),
[numeric GitHub identity claims](https://docs.cloud.google.com/iam/docs/workload-identity-federation-with-deployment-pipelines),
and [App Engine deployment permissions](https://docs.cloud.google.com/appengine/docs/standard/roles#recommended_role_for_application_deployment).
