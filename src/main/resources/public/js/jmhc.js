(function($) {

Backbone.$ = $;



window.RequestModel = Backbone.Model.extend({

    defaults : {
        concealed : [],
        concealed_kong : [],
        melded : [],
        winning_tile : "",
        winning_tile_origin : "Self Drawn",
        last_tile_situation : "Not Last Tile Situation",
        prevalent_wind : "EAST",
        player_wind : "EAST"
    },

    initialize : function RequestModel() {
        console.log('Doc Constructor');
    },

    add_concealed : function(tile){
        concealed.push(tile)
    },

    remove_concealed : function(index){
        concealed.splice(index,1)
    }

});

window.TileSelectorModel = Backbone.Model.extend({
   defaults: {
    b1: 4,
    b2: 4,
    b3: 4,
    b4: 4,
    b5: 4,
    b6: 4,
    b7: 4,
    b8: 4,
    b9: 4,

    c1: 4,
    c2: 4,
    c3: 4,
    c4: 4,
    c5: 4,
    c6: 4,
    c7: 4,
    c8: 4,
    c9: 4,

    s1: 4,
    s2: 4,
    s3: 4,
    s4: 4,
    s5: 4,
    s6: 4,
    s7: 4,
    s8: 4,
    s9: 4,

    dr:4,
    dg:4,
    dw:4,

    we:4,
    wn:4,
    ww:4,
    ws:4,

    ss:1,
    su:1,
    sa:1,
    sw:1,

    fp:1,
    fc:1,
    fb:1,
    fo:1

   },

   use : function(tile){
        var cpt = this.get(tile);
        if(cpt > 0) {
            this.set(tile,cpt -1)
            this.trigger("tileselector:used",tile)
        }
        else {
            this.trigger("error","tile has been used four times");
        }
   }


});

window.TileSelectorView = Backbone.View.extend({
    id: "tile_selector",

    initialize: function() {
        this.tile_template = _.template($("#tile-template").html());
        this.$el = $("#"+this.id);

        this.listenTo(this.model,'change',this.render);

        this.render();
    },

    render : function(){

        console.log("rendering TileSelectorView");

        this.$el.html("");

        //scoping in closure
        var outer = this;

        $.each(this.model.attributes,function(tile,cpt){
            outer.$el.append(outer.tile_template({tile:tile, cpt:cpt}));
        });

        return this;
    },

});

window.ConcealedModel = Backbone.Model.extend({
   defaults: {
    tiles:[]
   },

   add : function(tile){
     this.get("tiles").push(tile);
     this.trigger("change");
   }
});

window.ConcealedView = Backbone.View.extend({
    id: "concealed",

    initialize: function() {

        this.tileSelectorModel = this.options.tileSelectorModel;

        this.$el = $("#"+this.id);

        this.tile_template = _.template($("#tile-template").html());

        this.listenTo(this.tileSelectorModel,"tileselector:used",this.addTile);
        this.listenTo(this.model,"change",this.render);

        this.render();
    },

    render : function(){
        console.log("rendering ConcealedView");

        this.$el.html("");

        //scoping in closure
        var outer = this;

        $.each(this.model.get("tiles"),function(index,tile){
            outer.$el.append(outer.tile_template({tile:tile}));
        });

        return this;
    },

    addTile : function(tile){
        this.model.add(tile);
    }

});

var App = Backbone.Router.extend({

    initialize : function(){
      this.tileSelectorModel = new TileSelectorModel();
      this.tileSelectorView = new TileSelectorView({model: this.tileSelectorModel});

      this.concealedModel = new ConcealedModel();
      this.concealedView = new ConcealedView({
        model: this.concealedModel,
        tileSelectorModel:this.tileSelectorModel
      });

    },

    routes: {
        "tile_selector/select/:tile": "selectedTile"
    },

    selectedTile : function(tile){
        console.log("selectedTile : " + tile);
        this.tileSelectorModel.use(tile);
        //replace the route so that user can click again
        this.navigate("", {trigger: true, replace: true});
    }

});

$(document).ready(function(){

    //Instantiate Router
    new App();

    //This is needed for making router starts
    Backbone.history.start();

});


})(Zepto);