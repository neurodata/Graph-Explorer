
generateSidebar <- function()
{
	selectInput(inputId="genMethod",
	            label="Graph Generation Method",
					 choices=c('Erdos Renyi',
									'Barabasi',
									'Random Dot Product',
									'K Regular',
									'Ring',
									'Star',
									'Wheel',
									'Complete',
									'Complete Bipartite',
									'Random Bipartite',
									'Geometric Random Graph',
									'Watts Strogatz',
									'Growing Random Graph',
									'Stochastic Block Model',
									'Atlas',
									'Famous',
									'Tree',
									'Forest Fire',
									'Aging Prefatt',
									'de Bruijn',
									'Kautz'),
					 selected="Erdos Renyi")
}

generateParameters <- function()
{
    bsCollapse(
		 bsCollapsePanel(title="Parameters",
			  conditionalPanel(
			     condition="input.genMethod!='Famous' & input.genMethod!='Atlas' & input.genMethod!='Generated Worm' & input.genMethod!='de Bruijn'",
			  sliderInput(inputId='genN',label="n",min=10,max=10000,
			              value=100,step=10)),
			  conditionalPanel(
			     condition="input.genMethod != 'Watts Strogatz' & input.genMethod != 'Famous' & input.genMethod!='Atlas' & input.genMethod!='Generated Worm'",
				  checkboxInput(inputId='genDirected',label="Directed",
									 value=FALSE)),
		     conditionalPanel(
			     condition="input.genDirected == true",
				      conditionalPanel(
						   condition="input.genMethod=='Star' | input.genMethod=='Wheel'",
							  radioButtons(inputId='genMode',label='Mode',
											  choices=c("in", "out", "mutual"),
											  selected='out'))),
		     conditionalPanel(
			     condition="input.genMethod=='Erdos Renyi'",
				  sliderInput(inputId='genERP',label='p',min=0,max=1,
				              value=.1,step=0.01)),
		     conditionalPanel(
			     condition="input.genMethod=='Stochastic Block Model'",
				  sliderInput(inputId='genSBMk',label='# Of Blocks',min=2,max=10,
				              value=3,step=1),
			     checkboxInput(inputId='genDiagDom',label='Diagonally Dominant',
				                value=TRUE)),
		     conditionalPanel(
			     condition="input.genMethod=='Atlas'",
				  sliderInput(inputId='genAtlas',label='Graph Number',
				              min=1,max=1252,
				              value=10,step=1)),
		     conditionalPanel(
			     condition="input.genMethod=='Growing Random Graph'",
				  sliderInput(inputId='genGROWM',label='m',
				              min=0,max=10,
				              value=1,step=1),
				  checkboxInput(inputId='genGROWCitation',label='Citation',
				                value=TRUE)),
		     conditionalPanel(
			     condition="input.genMethod=='Tree'",
				  sliderInput(inputId='genTreeC',label='Children',
				              min=1,max=10,
				              value=1,step=1)),
		     conditionalPanel(
			     condition="input.genMethod=='Complete Bipartite'",
				  sliderInput(inputId='genCBPn',label='n2',
				              min=2,max=100,
				              value=1,step=1)),
		     conditionalPanel(
			     condition="input.genMethod=='Random Dot Product'",
				  radioButtons(inputId='genRDPMethod',label='Vector Generation',
				               choices=c("Dirichlet","Sphere"),
									selected="Dirichlet"),
				  sliderInput(inputId='genRDPDim',label='Dimension',
								  min=1,max=10,
								  value=2,step=1)),
		     conditionalPanel(
			     condition="input.genMethod=='Forest Fire'",
				  sliderInput(inputId='genFFfw',label='Forward Probability',
				              min=0,max=1,
				              value=.1,step=0.05),
				  sliderInput(inputId='genFFbf',label='Backward Factor',
				              min=0,max=1,
				              value=.1,step=0.05),
				  sliderInput(inputId='genFFAmbs',label='Ambassadors',
				              min=1,max=10,
				              value=1,step=1)),
		     conditionalPanel(
			     condition="input.genMethod=='Aging Prefatt'",
				  sliderInput(inputId='genAPm',label='Number of Edges',
				              min=1,max=50,
				              value=10,step=1),
				  sliderInput(inputId='genAPpaexp',label='Preference Exponent',
				              min=0,max=2,
				              value=1,step=.1),
				  sliderInput(inputId='genAPagingexp',label='Aging Exponent',
				              min=-10,max=0,
				              value=-1,step=.1),
				  sliderInput(inputId='genAPagingbin',label='Aging Bins',
				              min=10,max=1000,
				              value=10,step=10),
				  sliderInput(inputId='genAPdegcoef',label='Degree Coefficient',
				              min=0,max=10,
				              value=1,step=1),
				  sliderInput(inputId='genAPagecoef',label='Age Coefficient',
				              min=0,max=10,
				              value=1,step=1),
				  sliderInput(inputId='genAPzerodeg',label='Zero Degree Appeal',
				              min=.1,max=1,
				              value=1,step=.05),
				  sliderInput(inputId='genAPzeroage',label='Zero Age Appeal',
				              min=.1,max=1,
				              value=1,step=0.05),
				  sliderInput(inputId='genAPtimewindow',label='Time Window',
				              min=1,max=10,
				              value=1,step=1)),
		     conditionalPanel(
			     condition="input.genMethod=='Random Bipartite'",
				  sliderInput(inputId='genRBPn',label='n2',
				              min=2,max=100,
				              value=1,step=1),
				  sliderInput(inputId='genRBPP',label='p',min=0,max=1,
				              value=.1,step=0.01)),
		     conditionalPanel(
			     condition="input.genMethod=='de Bruijn'",
				  sliderInput(inputId='gendBn',label='n',min=2,max=5,
				              value=2,step=1),
				  sliderInput(inputId='gendBm',label='m',min=2,max=5,
				              value=2,step=1)
								  ),
		     conditionalPanel(
			     condition="input.genMethod=='Kautz'",
				  sliderInput(inputId='genKn',label='n',min=2,max=5,
				              value=2,step=1),
				  sliderInput(inputId='genKm',label='m',min=2,max=5,
				              value=2,step=1)
								  ),
		     conditionalPanel(
			     condition="input.genMethod=='Famous'",
				  selectInput(inputId='genFamous',label='Graph Name',
				              choices=famous.graphs,selected="Petersen")),
		     conditionalPanel(
			     condition="input.genMethod=='K Regular'",
				  sliderInput(inputId='genKRK',label='k',min=1,max=20,
				              value=5,step=1)),
		     conditionalPanel(
			     condition="input.genMethod=='Geometric Random Graph'",
				  sliderInput(inputId='genGRGRadius',label='Radius',min=0,max=1,
				              value=0.25,step=0.05),
				  checkboxInput(inputId="genGRGTorus",label="Torus",value=FALSE),
				  checkboxInput(inputId="genGRGcoords",label="Keep Coords",value=FALSE)
				  ),
		     conditionalPanel(
			     condition="input.genMethod=='Watts Strogatz'",
				  sliderInput(inputId='genWSDim',label='Dimension',min=1,max=3,
				              value=1,step=1),
				  sliderInput(inputId='genWSSize',label='Dimension',min=2,max=200,
				              value=100,step=1),
				  sliderInput(inputId='genWSNei',label='Dimension',min=2,max=100,
				              value=5,step=1),
				  sliderInput(inputId='genWSP',label='p',min=0,max=1,
				              value=.05,step=0.01)
				  ),
		     conditionalPanel(
			     condition="input.genMethod=='Barabasi'",
				  sliderInput(inputId='genBPower',label='Power',min=0,max=1,
				              value=1,step=.1),
				  sliderInput(inputId='genBM',label='m',min=1,max=10,
				              value=5,step=1),
				  sliderInput(inputId='genBZero',label='Zero Appeal',min=0,max=1,
				              value=1,step=0.01)
								  )
		 ))
}
