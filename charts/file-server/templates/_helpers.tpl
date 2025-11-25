{{/* Generate standard labels */}}
{{- define "file-server.labels" -}}
app: file-server-{{ .Values.environment }}
environment: {{ .Values.environment }}
{{- end -}}

{{/* Same for postgres */}}
{{- define "postgres.labels" -}}
app: postgres-{{ .Values.environment }}
{{- end -}}
