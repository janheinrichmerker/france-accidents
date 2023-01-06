[![CI](https://img.shields.io/github/actions/workflow/status/heinrichreimer/france-accidents/ci.yml?branch=main&style=flat-square)](https://github.com/heinrichreimer/france-accidents/actions/workflows/ci.yml)
[![Code coverage](https://img.shields.io/codecov/c/github/heinrichreimer/france-accidents?style=flat-square)](https://codecov.io/github/heinrichreimer/france-accidents/)
[![Issues](https://img.shields.io/github/issues/heinrichreimer/france-accidents?style=flat-square)](https://github.com/heinrichreimer/france-accidents/issues)
[![Commit activity](https://img.shields.io/github/commit-activity/m/heinrichreimer/france-accidents?style=flat-square)](https://github.com/heinrichreimer/france-accidents/commits)
[![License](https://img.shields.io/github/license/heinrichreimer/france-accidents?style=flat-square)](LICENSE)

# 🚨 france-accidents

Visualizing [accidents in France from 2005 to 2020](https://kaggle.com/ahmedlahlou/accidents-in-france-from-2005-to-2016)
, originally released
as [French Government open data](https://data.gouv.fr/en/datasets/bases-de-donnees-annuelles-des-accidents-corporels-de-la-circulation-routiere-annees-de-2005-a-2019/)
.

_The project is developed as part of
the [Information Retrieval and Visualization lecture](https://informatik.uni-halle.de/arbeitsgruppen/dbs/lehre/2757674_2757765/)
lecture at [Martin Luther University Halle-Wittenberg](https://uni-halle.de)._

## Usage

Start a web server with the visualizations running on http://localhost:8080/:

```shell
yarn start
```

Note that you must first [preprocess](#preprocessing) the data.

If you want to deploy the compiled, static resources to a HTTP server, run

```shell
yarn build
```

and copy the `dist/` folder to your web server's content root.

## Preprocessing

In order to fix various encoding and label issues and to combine the different open datasets,
we need to preprocess the data once before starting the web server.
Here are the steps required:

1. Install [Python 3](https://python.org/downloads/)
2. Install [pipx](https://pipxproject.github.io/pipx/installation/#install-pipx)
3. Install [Pipenv](https://pipenv.pypa.io/en/latest/install/#isolated-installation-of-pipenv-with-pipx)
4. Install dependencies (this may take a while):
    ```shell
    pipenv install
    ```
5. Run preprocessing:
    ```shell
    pipenv run python preprocessing/preprocess.py 
    ```

### Sampling for testing

To randomly sample a smaller test dataset for testing purposes, run the following:
```shell
shuf -n 10000 static/data/accidents.jsonl > static/data/accidents-sample.jsonl
```

## Ideas
1. time series
    - x-axis: time
    - interaction: strip year or not
      (might be possible to detect yearly trends)
    - y-axis: number of injuries/fatalities
    - interaction: switch between injuries and fatalities
    - banking to 45 degrees
    - interaction: toggle color scale of line:
      - none
      - birth year
    - interaction: filter for different safety equipment
2. icon-based (stick figures) für personen
    - filter for drivers/passengers
    - maybe more filtering
    - young vs. old drivers
    - sex
    - reason for travelling
    - safety equipment
    - alone vs. accompanied by
    - plot in geographical coordinates
      - grid every ?? kilometers
      - interaction: change aggregation type
        - average values and plot one single stick figure per grid cell
        - plot each incident as one stick figure but overlay with x-ray technique
          - that way no detail is lost by aggregation
    - interaction: additional color dimension
      - proportion of persons injured or dead
3. treemap (or tree) of accident types:
    - collision types
    - road category
    - light conditions
    - weather
    - intersection type
    - road curvature
    - vehicle type
    - situation
    - interact: switch between tree and treemap


## Questions
- Are there repeating yearly patterns? Are roads more dangerous in winter?
- Do older or younger drivers drive more safely?
- Where are more / the more severe accidents? city or rural areas?
- Do the proportion of dead and injured persons correlate? Where are they most different?
- Do dedicated bicycle lanes make roads safer for cyclists?
  
## License

This project is [MIT licensed](LICENSE), you can use it for whatever you want as long as you mention this repository.
We use the [Elm](https://reactjs.org/) framework which is
also [open source](https://github.com/elm/core/blob/master/LICENSE).
