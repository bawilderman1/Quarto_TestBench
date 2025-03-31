function() {
  return '<span style="color:' + this.series.color + ';">●</span>' + this.series.name + ': ' +
       '<b>' +  new Intl.NumberFormat('en-US', { style: 'currency', currency: 'USD' }).format(this.y) + '</b>'; 
}