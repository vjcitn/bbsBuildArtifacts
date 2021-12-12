# bbsBuildArtifacts

Analyze a slice of Bioc Build System artifacts

See the "Get started" tab above for general information.

As of June 17 2021, we need development in the following areas:

- use `get_events` to parse R CMD check logs, extract key elements and format for reporting
    - note that BiocQE has `rcc_to_dataframes` available
    - BiocQE can produce/manage a SQLite database with `rcc_to_dataframes`
- extend `get_events` to work with outputs of `BiocCheck::BiocCheck`
- using source folders (perhaps using `BiocQE::populate_local_gits`), compute `pkgnet`
metrics and figures
    - test coverage summaries
    - dependency analysis and control
- consider how to organize the information in a snapshot for decisionmaking
    - would semantic analysis of check logs (errors, warnings) be useful?  word cloud?
- consider how to organize the information in a series of snapshots for reasoning
about software ecosystem dynamics
- how can we use information of this sort to define priorities for maintenance?
