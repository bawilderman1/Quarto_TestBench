function () {
  return 'Open: <b>' + new Intl.NumberFormat('en-US', { style: 'currency', currency: 'USD' }).format(this.open) + '</b> <br/>' +
       'High: <b>' + new Intl.NumberFormat('en-US', { style: 'currency', currency: 'USD' }).format(this.high) + '</b> <br/>' +
       'Low: <b>' + new Intl.NumberFormat('en-US', { style: 'currency', currency: 'USD' }).format(this.low) + '</b> <br/>' +
       'Close: <b>' + new Intl.NumberFormat('en-US', { style: 'currency', currency: 'USD' }).format(this.close) + '</b> <br/>';  
}