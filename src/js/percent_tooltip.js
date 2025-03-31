function() {
  return '<span style="color:' + this.series.color + ';">●</span>' + this.series.name + ': ' +
       '<b>' + new Intl.NumberFormat('en-US', {style: 'percent', minimumFractionDigits: 2, maximumFractionDigits: 2}).format(this.y) + '</b>';
}