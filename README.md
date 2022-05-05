# GCS Data Portal

This webapp allows users from all government organisations to submit HR data to the Government Communications Service. It is hosted on PaaS. 

## Running the webapp

To run the webapp you must have downloaded the CloudFoundry Command Line Interface. You must also have a PaaS (Platform as a Service) account to authenticate your CloudFoundry session. 

While authenticated on CloudFoundry, navigate to the top level directory in this repository. Enter the command ```cf push```. The app should now be running on the specified CloudFoundry route.

## TODO

### Critical 

- Complete data checking
  - Check that all columns are present.
  - Reject if extra columns exist that are not expected.
- Setup dependabot

### Nice to have

- Make the webapp more visually appealing
- Track usage
