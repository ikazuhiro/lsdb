case $_
when /^\t(\* ([^:]+): )\* ([^: ]+)/
  data, files, file = $~, $2, $3
  if files.split(/, /).member?(file)
    $_.slice!(data.begin(1) .. data.end(1) - 1)
  end
when /^\d{4}-\d{2}-\d{2}( \d{2}:\d{2})/
  $_.slice!($~.begin(1) .. $~.end(1) - 1)
end
